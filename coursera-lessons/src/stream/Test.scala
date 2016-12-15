package stream

object Test extends App {

  val range = Stream.range(1, 10)
  range.tail
  range.tail.tail

  println("-----")

  val infinite = Stream.from(1)
  infinite.tail
  infinite.tail.tail

  println("-----")

  val infiniteBy2 = Stream.map(infinite, _ * 2)
  infiniteBy2.tail
  infiniteBy2.tail.tail

}
