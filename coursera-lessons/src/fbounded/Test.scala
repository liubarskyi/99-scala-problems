package fbounded

object Test extends App {

  trait Fruit[T <: Fruit[T]] {
    def compareTo(that: T): Int = 0
  }

  class Apple extends Fruit[Apple]
  class AppleSub extends Apple

  class Orange extends Fruit[Orange]
  class OrangeSub extends Orange

  val apple = new Apple
  val appleSub = new AppleSub
  val orange = new Orange
  val orangeSub = new OrangeSub

  apple.compareTo(appleSub)
  orange.compareTo(orangeSub)
  //  apple.compare(orange))
}
