package forexpressions

object Test extends App {

  val filtered = for (i <- 1 to 10; j <- 1 to i) yield (i, j)
  println(filtered)

}
