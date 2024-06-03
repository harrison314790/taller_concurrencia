package taller4

import org.scalameter._
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global

object Benchmarking {

  val smallRange = Gen.range("smallRange")(0, 1000000, 100000)
  val largeRange = Gen.range("largeRange")(0, 10000000, 1000000)

  val f = Suma(Prod(Atomo("x"), Atomo("x")), Numero(1)) // f(x) = x^2 + 1
  val x0 = 10.0
  val ba = (x: Double) => math.abs(x) < 1e-7

  def main(args: Array[String]): Unit = {
    val seqTime = withWarmer(new Warmer.Default) measure {
      smallRange.foreach { _ =>
        Newton.raizNewton(f, Atomo("x"), x0, ba)
      }
    }

    val parTime = withWarmer(new Warmer.Default) measure {
      smallRange.foreach { _ =>
        Await.result(NewtonParalela.raizNewtonPar(f, Atomo("x"), x0, ba), Duration.Inf)
      }
    }

    println(s"Sequential time: $seqTime")
    println(s"Parallel time: $parTime")
  }
}
