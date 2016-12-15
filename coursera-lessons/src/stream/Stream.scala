package stream

sealed trait Stream[+T] {

  def isEmpty: Boolean

  def head: T

  def tail: Stream[T]

}

object Stream {

  def cons[T](h: T, t: => Stream[T]): Stream[T] = new Stream[T] {

    println(h + " was evaluated")

    def isEmpty: Boolean = false

    def head: T = h

    lazy val tail: Stream[T] = t

  }

  val empty = new Stream[Nothing] {

    def isEmpty: Boolean = true

    def head: Nothing = throw new UnsupportedOperationException

    def tail: Stream[Nothing] = throw new UnsupportedOperationException

  }

  def range(from: Int, to: Int): Stream[Int] = {
    if (from == to) empty
    else cons(from, range(from + 1, to))
  }

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def map(stream: Stream[Int], f: Int => Int): Stream[Int] = cons(f(stream.head), map(stream.tail, f))

}