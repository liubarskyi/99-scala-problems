package typelinearization

object Test extends App {

  trait A {
    def foo(): Unit = println("A")
  }

  trait B extends A {
    override def foo(): Unit = {
      println("B")
      super.foo()
    }
  }

  trait C extends A {
    override def foo(): Unit = {
      println("C")
      super.foo()
    }
  }

  class D1 extends B with C

  class D2 extends C with B

  new D1().foo()
  println("------")
  new D2().foo()

}
