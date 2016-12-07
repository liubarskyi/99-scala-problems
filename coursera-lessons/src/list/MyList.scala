package list

trait MyList[+T] {
  def isEmpty: Boolean

  def head: T

  def tail: MyList[T]

  def prepend[U >: T](u: U): MyList[U] = new Cons(u, this)

  override def toString = head + " :: " + tail
}
