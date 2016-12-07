package natural

class Succ(val predecessor: Nat) extends Nat {
  def isZero: Boolean = false

  def successor: Nat = new Succ(this)

  def +(that: Nat): Nat = this.successor + that.predecessor

  def -(that: Nat): Nat = if (that.isZero) this else this.predecessor - that.predecessor
}
