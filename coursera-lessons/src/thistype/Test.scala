package thistype

import typelinearization.Test.{D1, D2}

object Test extends App {

  trait A {
    def foo(): this.type
  }

  trait B extends A

  class C extends B {
    override def foo(): this.type = this
  }

  println(new C().foo())

}
