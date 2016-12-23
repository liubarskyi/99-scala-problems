package calculator

sealed abstract class Expr

final case class Literal(v: Double) extends Expr

final case class Ref(name: String) extends Expr

final case class Plus(a: Expr, b: Expr) extends Expr

final case class Minus(a: Expr, b: Expr) extends Expr

final case class Times(a: Expr, b: Expr) extends Expr

final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {

  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions map { case (name, expr) => (name, Signal(eval(expr.apply(), namedExpressions))) }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = eval(expr, references, Set())

  private def eval(expr: Expr, references: Map[String, Signal[Expr]], observed: Set[String]): Double = expr match {
    case Literal(v) => v
    case Ref(name) if observed contains name => Double.NaN
    case Ref(name) => eval(getReferenceExpr(name, references), references, observed + name)
    case Plus(a, b) => eval(a, references, observed) + eval(b, references, observed)
    case Minus(a, b) => eval(a, references, observed) - eval(b, references, observed)
    case Times(a, b) => eval(a, references, observed) * eval(b, references, observed)
    case Divide(a, b) => eval(a, references, observed) / eval(b, references, observed)
  }

  /** Get the Expr for a referenced variables.
    * If the variable is not known, returns a literal NaN.
    */
  private def getReferenceExpr(name: String, references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
  
}
