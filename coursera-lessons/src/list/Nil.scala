package list

object Nil extends MyList[Nothing] {
  def isEmpty = true

  def head = throw new NoSuchMethodException

  def tail = throw new NoSuchElementException

  override def toString = "Nil"
}
