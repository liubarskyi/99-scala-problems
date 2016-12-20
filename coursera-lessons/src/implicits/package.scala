
package object implicits {

  //  trait Printable[P] {
  //    def prepare(p: P): String
  //  }
  //
  //  implicit class PrintableOps[P: Printable](p: P) {
  //    def prepare = implicitly[Printable[P]].prepare(p)
  //  }
  //
  //  implicit object PrintableBoolean extends Printable[Boolean] {
  //    override def prepare(b: Boolean): String = "Boolean." + b.toString
  //  }

  implicit class PrintableBoolean(b: Boolean) {
    def prepare = "PrintableBoolean." + b.toString
  }

}
