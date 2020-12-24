package retypecheck

import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.async.Async._
import scala.concurrent.Future

class TyperWithScalaAsyncSpec extends AsyncFlatSpec with Matchers {
  behavior of "Typer for code using Scala Async examples"

  it should "correctly compile async block with await calls" in {
    val v1 = TyperTester.retyper {
      def slowCalcFuture: Future[Int] = Future.successful(21)
      async { await(slowCalcFuture) + await(slowCalcFuture) }
    }

    val v2 = TyperTester.retyperAll {
      def slowCalcFuture: Future[Int] = Future.successful(21)
      async { await(slowCalcFuture) + await(slowCalcFuture) }
    }

    @TyperTester.retyper object o1 {
      def slowCalcFuture: Future[Int] = Future.successful(21)
      val v = async { await(slowCalcFuture) + await(slowCalcFuture) }
    }

    @TyperTester.retyperAll object o2 {
      def slowCalcFuture: Future[Int] = Future.successful(21)
      val v = async { await(slowCalcFuture) + await(slowCalcFuture) }
    }

    Future.sequence(Seq(v1, v2, o1.v, o2.v)) map { values =>
      (values map { _ should be (42) }).head
    }
  }
}
