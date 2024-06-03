package taller4

sealed trait Expr
case class Numero(value: Double) extends Expr
case class Atomo(symbol: String) extends Expr
case class Suma(left: Expr, right: Expr) extends Expr
case class Prod(left: Expr, right: Expr) extends Expr
case class Resta(left: Expr, right: Expr) extends Expr
case class Div(left: Expr, right: Expr) extends Expr
case class Expo(base: Expr, exp: Expr) extends Expr
case class Logaritmo(base: Expr, arg: Expr) extends Expr

object Newton {
  def mostrar(expr: Expr): String = expr match {
    case Numero(value) => value.toString
    case Atomo(symbol) => symbol
    case Suma(left, right) => s"(${mostrar(left)} + ${mostrar(right)})"
    case Prod(left, right) => s"(${mostrar(left)} * ${mostrar(right)})"
    case Resta(left, right) => s"(${mostrar(left)} - ${mostrar(right)})"
    case Div(left, right) => s"(${mostrar(left)} / ${mostrar(right)})"
    case Expo(base, exp) => s"(${mostrar(base)} ^ ${mostrar(exp)})"
    case Logaritmo(base, arg) => s"log_${mostrar(base)}(${mostrar(arg)})"
  }

  def derivar(expr: Expr, atomo: Atomo): Expr = expr match {
    case Numero(_) => Numero(0)
    case `atomo` => Numero(1)
    case Suma(left, right) => Suma(derivar(left, atomo), derivar(right, atomo))
    case Resta(left, right) => Resta(derivar(left, atomo), derivar(right, atomo))
    case Prod(left, right) => Suma(Prod(derivar(left, atomo), right), Prod(left, derivar(right, atomo)))
    case Div(left, right) => Div(Resta(Prod(derivar(left, atomo), right), Prod(left, derivar(right, atomo))), Prod(right, right))
    case Expo(base, exp) => Prod(Expo(base, exp), Suma(Prod(derivar(base, atomo), Div(exp, base)), Prod(derivar(exp, atomo), Logaritmo(Numero(math.E), base))))
    case Logaritmo(base, arg) => Div(derivar(arg, atomo), Prod(arg, Logaritmo(Numero(math.E), base)))
    case _ => throw new UnsupportedOperationException(s"Cannot derive expression: $expr")
  }

  def evaluar(expr: Expr, atomo: Atomo, value: Double): Double = expr match {
    case Numero(v) => v
    case `atomo` => value
    case Suma(left, right) => evaluar(left, atomo, value) + evaluar(right, atomo, value)
    case Prod(left, right) => evaluar(left, atomo, value) * evaluar(right, atomo, value)
    case Resta(left, right) => evaluar(left, atomo, value) - evaluar(right, atomo, value)
    case Div(left, right) => evaluar(left, atomo, value) / evaluar(right, atomo, value)
    case Expo(base, exp) => math.pow(evaluar(base, atomo, value), evaluar(exp, atomo, value))
    case Logaritmo(base, arg) => math.log(evaluar(arg, atomo, value)) / math.log(evaluar(base, atomo, value))
  }

  def limpiar(expr: Expr): Expr = expr match {
    case Suma(Numero(0), right) => limpiar(right)
    case Suma(left, Numero(0)) => limpiar(left)
    case Prod(Numero(1), right) => limpiar(right)
    case Prod(left, Numero(1)) => limpiar(left)
    case Prod(Numero(0), _) => Numero(0)
    case Prod(_, Numero(0)) => Numero(0)
    case Resta(left, Numero(0)) => limpiar(left)
    case Div(left, Numero(1)) => limpiar(left)
    case Suma(left, right) => Suma(limpiar(left), limpiar(right))
    case Prod(left, right) => Prod(limpiar(left), limpiar(right))
    case Resta(left, right) => Resta(limpiar(left), limpiar(right))
    case Div(left, right) => Div(limpiar(left), limpiar(right))
    case Expo(base, exp) => Expo(limpiar(base), limpiar(exp))
    case Logaritmo(base, arg) => Logaritmo(limpiar(base), limpiar(arg))
    case other => other
  }

  def raizNewton(f: Expr, a: Atomo, x0: Double, ba: Double => Boolean, maxIter: Int = 100, tol: Double = 1e-7): Option[Double] = {
    def newton(x: Double, iter: Int): Option[Double] = {
      if (iter >= maxIter) None
      else {
        val fx = evaluar(f, a, x)
        if (ba(fx)) Some(x)
        else {
          val dfx = evaluar(derivar(f, a), a, x)
          if (dfx == 0) None
          else {
            val x1 = x - fx / dfx
            if (math.abs(x1 - x) < tol) Some(x1)
            else newton(x1, iter + 1)
          }
        }
      }
    }
    newton(x0, 0)
  }
}
