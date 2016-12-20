package option

sealed trait Option[+A] {
  def flatMap[B](f: A => Option[B]): Option[B] = Option.flatMap(this, f)
}

case class Some[A](a: A) extends Option[A]

object None extends Option[Nothing]

object Option {

  def map[A, B](option: Option[A], f: A => B): Option[B] = option match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatten[A](option: Option[A]): A = option match {
    case Some(a) => a
  }

  def flatMap[A, B](option: Option[A], f: A => Option[B]): Option[B] = flatten(map(option, f))

}
