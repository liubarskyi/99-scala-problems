package natural

object Zero extends Nat {
  def isZero: Boolean = true

  def predecessor: Nat = throw new NoSuchElementException

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if (that.isZero) Zero else throw new NoSuchElementException
}
