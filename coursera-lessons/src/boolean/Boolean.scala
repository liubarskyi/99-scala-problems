package boolean

abstract class Boolean {

  def ifThenElse(t: => Boolean, e: => Boolean): Boolean

  def &&(that: Boolean) = ifThenElse(that, False)

  def ||(that: Boolean) = ifThenElse(True, that)

  def unary_! = ifThenElse(False, True)

  def ==(that: Boolean) = ifThenElse(that, !that)

  def !=(that: Boolean) = ifThenElse(!that, that)

  def <(that: Boolean) = ifThenElse(False, that)
}

object True extends Boolean {
  def ifThenElse(t: => Boolean, e: => Boolean): Boolean = t

  override def toString: String = "true"

}

object False extends Boolean {
  def ifThenElse(t: => Boolean, e: => Boolean): Boolean = e

  override def toString: String = "false"
}
