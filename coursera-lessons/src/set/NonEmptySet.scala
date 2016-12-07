package set


class NonEmptySet(x: Int, left: IntSet, right: IntSet) extends IntSet {

  override def contains(x: Int): Boolean = {
    if (x < this.x) left contains x
    else if (x > this.x) right contains x
    else true
  }

  override def include(x: Int): IntSet = {
    if (x < this.x) new NonEmptySet(this.x, left include x, right)
    else if (x > this.x) new NonEmptySet(this.x, left, right include x)
    else this
  }

  override def union(that: IntSet): IntSet = {
    ((left union right) union that) include x
  }

  override def toString = "[" + left + x + right + "]"

}
