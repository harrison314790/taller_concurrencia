package taller4

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object NewtonParalela {
  import Newton._

  def mostrarPar(expr: Expr): String = mostrar(expr)  // Reutilizamos la implementación de mostrar

  def derivarPar(expr: Expr, atomo: Atomo): Future[Expr] = Future {
    derivar(expr, atomo)
  }

  def evaluarPar(expr: Expr, atomo: Atomo, value: Double): Future[Double] = Future {
    evaluar(expr, atomo, value)
  }

  def limpiarPar(expr: Expr): Future[Expr] = Future {
    limpiar(expr)
  }

  def raizNewtonPar(f: Expr, a: Atomo, x0: Double, ba: Double => Boolean, maxIter: Int = 100, tol: Double = 1e-7): Future[Option[Double]] = {
    def newton(x: Double, iter: Int): Future[Option[Double]] = {
      if (iter >= maxIter) Future.successful(None)
      else {
        val fxFut = evaluarPar(f, a, x)
        fxFut.flatMap { fx =>
          if (ba(fx)) Future.successful(Some(x))
          else {
            val dfxFut = evaluarPar(derivar(f, a), a, x)
            dfxFut.flatMap { dfx =>
              if (dfx == 0) Future.successful(None)
              else {
                val x1 = x - fx / dfx
                if (math.abs(x1 - x) < tol) Future.successful(Some(x1))
                else newton(x1, iter + 1)
              }
            }
          }
        }
      }
    }
    newton(x0, 0)
  }
}


