package set


abstract class IntSet {
  def contains(x: Int): Boolean

  def include(x: Int): IntSet

  def union(that: IntSet): IntSet
}
