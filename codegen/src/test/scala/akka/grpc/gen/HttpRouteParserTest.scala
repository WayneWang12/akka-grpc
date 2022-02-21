package akka.grpc.gen

import com.google.api.HttpRule
import com.google.protobuf.Descriptors.{Descriptor, FieldDescriptor}
import org.mockito.Mockito.when
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.mockito.MockitoSugar

import java.util

class HttpRouteParserTest extends AnyWordSpec with Matchers with MockitoSugar {
  "Path template parse" should {
    "parse grpc path to akka route" in {
      val grpcPath = "/test/abc/{name=message/*}"
      val httpRule = HttpRule.newBuilder().setGet(grpcPath).setBody("test").build()
      val mockDescriptor = mock[Descriptor]
      val mockField1 = mock[FieldDescriptor]
      val mockField2 = mock[FieldDescriptor]
      val mockFields = {
        val list = new util.ArrayList[FieldDescriptor]()
        list.add(mockField1)
        when(mockField1.getName).thenReturn("test")
        when(mockField2.getName).thenReturn("query")
        list.add(mockField2)
        list
      }
      when(mockDescriptor.findFieldByName("test")).thenReturn(mockField1)
      when(mockDescriptor.getFields).thenReturn(mockFields)
      val result = HttpRuleParser.parse(grpcPath, httpRule, mockDescriptor)
      println(result.akkaRoute)
      result.akkaRoute shouldBe """path( "test" / "abc" / grpcVariable( "message" / Segment )) & parameters("query".?)"""
    }
  }
}
