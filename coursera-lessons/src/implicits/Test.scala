package implicits

object Test extends App {

  def myPrint(b: Boolean) = println(b.prepare)

  myPrint(false)

}
