package categories

object Functions extends App {

  def id[T](x: T): T = x

  def compose[A, B, C](f: A => B, g: B => C): A => C = (a: A) => g(f(a))

  def memorize[A, B](f: A => B): A => B = new Function1[A, B] {
    private val cache: scala.collection.mutable.Map[A, B] = scala.collection.mutable.Map()

    def apply(a: A): B = cache.getOrElseUpdate(a, f(a))
  }

  def factorial(n: Int): Int = {
    def recursive(n: Int): Int = {
      if (n == 1) 1
      else n * recursive(n - 1)
    }
    println(s"factorial($n)")
    recursive(n)
  }

  val factorialMemo = memorize(factorial)
  println(factorial(10))
  println(factorial(10))
  println("-----")
  println(factorialMemo(10))
  println(factorialMemo(10))

}
