package akka.grpc

import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.server.{ PathMatcher, PathMatcher1 }

import scala.annotation.tailrec

object GrpcPathMatchers {
  import PathMatcher._
  def grpcVariable[T](matcher: PathMatcher[T]): PathMatcher1[String] = {
    new PathMatcher1[String]() {
      override def apply(path: Path): Matching[Tuple1[String]] = {
        matcher(path) match {
          case Matched(pathRest, _) =>
            @tailrec
            def loop(rest: Path, acc: String): String = {
              if (rest.tail == pathRest) acc
              else {
                rest match {
                  case Path.Segment(head, tail) =>
                    loop(tail, acc + head)
                  case p =>
                    loop(p.tail, acc + "/")
                }
              }
            }
            val str = if (pathRest.isEmpty) path.toString() else loop(path, "")
            Matched(pathRest, Tuple1(str))
          case PathMatcher.Unmatched =>
            Unmatched
        }
      }
    }
  }
}
