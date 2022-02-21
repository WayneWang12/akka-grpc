package akka.grpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class GrpcPathMatchersTest extends AnyWordSpec with Matchers with ScalatestRouteTest {

  import GrpcPathMatchers._

  "Grpc Path Matchers" should {
    "match grpc path" in {

      val route = path(grpcVariable("a" / "b")) {
        complete(_)
      } ~
        (path("test" / "abc" / grpcVariable("message" / Segment)) & parameters("query".?) & get) { (a, b) =>
          complete(s"$a, $b")
        }

      Get("/test/abc/message/abc?query=123") ~> route ~> check {
        responseAs[String] shouldEqual "a/b"
      }

    }
  }
}
