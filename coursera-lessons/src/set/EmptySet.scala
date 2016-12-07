package set


class EmptySet extends IntSet {
  override def contains(x: Int): Boolean = false

  override def include(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)

  override def union(that: IntSet): IntSet = that

  override def toString = "."
}
