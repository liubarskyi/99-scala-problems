package expression

trait Expression {

  def evaluate: Int = {
    this match {
      case Number(n) => n
      case Variable(v) => throw new UnsupportedOperationException
      case Sum(left, right) => left.evaluate + right.evaluate
      case Product(left, right) => left.evaluate * right.evaluate
    }
  }

  def show: String = {
    this match {
      case Number(n) => n.toString
      case Variable(v) => v
      case Sum(left, right) => left.show + "+" + right.show
      case Product(left, right) => showWithParenthesesIfNeeded(left) + "*" + showWithParenthesesIfNeeded(right)
    }
  }

  def showWithParenthesesIfNeeded(expression: Expression): String = {
    expression match {
      case sum: Sum => "(" + sum.show + ")"
      case _ => expression.show
    }
  }

}
