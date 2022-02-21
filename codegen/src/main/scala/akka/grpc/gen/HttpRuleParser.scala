package akka.grpc.gen

import com.google.api.HttpRule
import com.google.protobuf.Descriptors.Descriptor

import scala.jdk.CollectionConverters.asScalaBufferConverter
import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{ CharSequenceReader, Positional }

object HttpRuleParser extends Parsers {

  override type Elem = Char

  final class ParsedTemplate(val path: String, template: Template, rule: HttpRule, input: Descriptor) {
    lazy val akkaRoute: String = {
      def doToAkkaRoute(builder: StringBuilder, segments: List[Segment]): StringBuilder =
        segments match {
          case Nil => builder
          case head :: tail =>
            head match {
              case LiteralSegment(literal) =>
                builder.append(s""" "$literal"""")
              case SingleSegmentMatcher =>
                builder.append(" Segment ")
              case MultiSegmentMatcher() =>
                builder.append(" Remaining ")
              case VariableSegment(_, maybeTemplate) =>
                builder.append(" grpcVariable(")
                maybeTemplate.map(template => doToAkkaRoute(builder, template)).getOrElse(builder.append(" Segment"))
                builder.append(s")")
            }

            if (tail.nonEmpty) builder.append(" /")

            doToAkkaRoute(builder, tail)
        }

      def doToQuery(builder: StringBuilder, input: Descriptor): Unit = {
        rule.getBody match {
          case "" | "*" => input -> Seq.empty
          case fieldName =>
            val field = input.findFieldByName(fieldName)
            if (field == null)
              throw new Exception(s"Body configured to [$fieldName] but that field does not exist on input type.")
            else if (field.isRepeated)
              throw new Exception(s"Body configured to [$fieldName] but that field is a repeated field.")
            else
              field.getMessageType
            val pathFields = fields.flatMap(_.fieldPath)
            val remainingQueries =
              input.getFields.asScala.filter(field => field.getName != fieldName && !pathFields.contains(field.getName))
            val queries = remainingQueries
              .map { field =>
                s""""${field.getName}".?"""
              }
              .mkString(",")
            if (queries.nonEmpty) {
              builder.append(" & parameters(")
              builder.append(queries)
              builder.append(")")
            }
        }
      }

      val builder = new StringBuilder
      builder.append("path(")
      doToAkkaRoute(builder, template.segments)
      builder.append(")")
      doToQuery(builder, input)
      builder.toString()
    }

    val fields: List[TemplateVariable] = {
      var found = Set.empty[List[String]]
      template.segments.collect {
        case v @ VariableSegment(fieldPath, _) if found(fieldPath) =>
          throw PathTemplateParseException("Duplicate path in template", path, v.pos.column + 1)
        case VariableSegment(fieldPath, segments) =>
          found += fieldPath
          TemplateVariable(
            fieldPath,
            segments.exists(_ match {
              case ((_: MultiSegmentMatcher) :: _) | (_ :: _ :: _) => true
              case _                                               => false
            }))
      }
    }
  }

  final case class TemplateVariable(fieldPath: List[String], multi: Boolean)

  final case class PathTemplateParseException(msg: String, path: String, column: Int)
      extends RuntimeException(
        s"$msg at ${if (column >= path.length) "end of input" else s"character $column"} of '$path'") {

    def prettyPrint: String = {
      val caret =
        if (column >= path.length) ""
        else "\n" + path.take(column - 1).map { case '\t' => '\t'; case _ => ' ' } + "^"

      s"$msg at ${if (column >= path.length) "end of input" else s"character $column"}:${'\n'}$path$caret"
    }
  }

  final def parse(path: String, rule: HttpRule, input: Descriptor): ParsedTemplate =
    template(new CharSequenceReader(path)) match {
      case Success(template, _) =>
        new ParsedTemplate(path, validate(path, template), rule, input)
      case NoSuccess(msg, next) =>
        throw PathTemplateParseException(msg, path, next.pos.column)
    }

  final private def validate(path: String, template: Template): Template = {
    def flattenSegments(segments: Segments, allowVariables: Boolean): Segments =
      segments.flatMap {
        case variable: VariableSegment if !allowVariables =>
          throw PathTemplateParseException("Variable segments may not be nested", path, variable.pos.column)
        case VariableSegment(_, Some(nested)) => flattenSegments(nested, false)
        case other                            => List(other)
      }

    // Flatten, verifying that there are no nested variables
    val flattened = flattenSegments(template.segments, true)

    // Verify there are no ** matchers that aren't the last matcher
    flattened.dropRight(1).foreach {
      case m @ MultiSegmentMatcher() =>
        throw PathTemplateParseException(
          "Multi segment matchers (**) may only be in the last position of the template",
          path,
          m.pos.column)
      case _ =>
    }
    template
  }

  // AST for syntax described here:
  // https://cloud.google.com/endpoints/docs/grpc-service-config/reference/rpc/google.api#google.api.HttpRule.description.subsection
  // Note that there are additional rules (eg variables cannot contain nested variables) that this AST doesn't enforce,
  // these are validated elsewhere.
  final private case class Template(segments: Segments, verb: Option[Verb])

  private type Segments = List[Segment]
  private type Verb = String

  sealed private trait Segment

  final private case class LiteralSegment(literal: Literal) extends Segment

  final private case class VariableSegment(fieldPath: FieldPath, template: Option[Segments])
      extends Segment
      with Positional

  private type FieldPath = List[Ident]

  private case object SingleSegmentMatcher extends Segment

  final private case class MultiSegmentMatcher() extends Segment with Positional

  private type Literal = String
  private type Ident = String

  final private val NotLiteral = Set('*', '{', '}', '/', ':', '\n')

  // Matches ident syntax from https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
  final private val ident: Parser[Ident] = rep1(
    acceptIf(ch => (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z'))(e =>
      s"Expected identifier first letter, but got '$e'"),
    acceptIf(ch => (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z') || (ch >= '0' && ch <= '9') || ch == '_')(_ =>
      "identifier part")) ^^ (_.mkString)

  // There is no description of this in the spec. It's not a URL segment, since the spec explicitly says that the value
  // must be URL encoded when expressed as a URL. Since all segments are delimited by a / character or a colon, and a
  // literal may only be a full segment, we could assume it's any non slash or colon character, but that would mean
  // syntax errors in variables for example would end up being parsed as literals, which wouldn't give nice error
  // messages at all. So we'll be a little more strict, and not allow *, { or } in any literals.
  final private val literal: Parser[Literal] = rep(acceptIf(ch => !NotLiteral(ch))(_ => "literal part")) ^^ (_.mkString)

  final private val fieldPath: Parser[FieldPath] = rep1(ident, '.' ~> ident)

  final private val literalSegment: Parser[LiteralSegment] = literal ^^ LiteralSegment

  // After we see an open curly, we commit to erroring if we fail to parse the remainder.
  final private def variable: Parser[VariableSegment] =
    positioned(
      '{' ~> commit(
        fieldPath ~ ('=' ~> segments).? <~ '}'.withFailureMessage("Unclosed variable or unexpected character") ^^ {
          case fieldPath ~ maybeTemplate => VariableSegment(fieldPath, maybeTemplate)
        }))

  final private val singleSegmentMatcher: Parser[SingleSegmentMatcher.type] = '*' ^^ (_ => SingleSegmentMatcher)
  final private val multiSegmentMatcher: Parser[MultiSegmentMatcher] = positioned(
    '*' ~ '*' ^^ (_ => MultiSegmentMatcher()))
  final private val segment: Parser[Segment] = commit(
    multiSegmentMatcher | singleSegmentMatcher | variable | literalSegment)

  final private val verb: Parser[Verb] = ':' ~> literal
  final private val segments: Parser[Segments] = rep1(segment, '/' ~> segment)
  final private val endOfInput: Parser[None.type] = Parser { in =>
    if (!in.atEnd)
      Error("Expected '/', ':', path literal character, or end of input", in)
    else
      Success(None, in)
  }

  final private val template: Parser[Template] = '/'.withFailureMessage("Template must start with a slash") ~>
    segments ~ verb.? <~ endOfInput ^^ {
      case segments ~ maybeVerb => Template(segments, maybeVerb)
    }
}
