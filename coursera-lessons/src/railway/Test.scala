package railway

object Test extends App {

  def validateNotEmpty(user: String): Option[String] = user match {
    case "" => None
    case _ => Some(user)
  }

  def validateAdmin(user: String): Option[String] = user.toLowerCase match {
    // imitates request to database
    case "admin" | "administrator" => Some(user)
    case _ => None
  }

  def bind[A, B](f: A => Option[B]): Option[A] => Option[B] = {
    def binding(o: Option[A]): Option[B] = o match {
      case None => None
      case Some(a) => f(a)
    }
    binding
  }

  def compose[A, B, C](f: A => Option[B], g: B => Option[C]): A => Option[C] = {
    def composition(a: A): Option[C] = f(a) match {
      case None => None
      case Some(b) => g(b)
    }
    composition
  }

  def validateTrueAdmin(user: String) = compose(validateNotEmpty, validateAdmin)

  println(validateAdmin(""))
  println(validateAdmin("adm"))
  println(validateAdmin("admin1"))
  println(validateAdmin("admin"))

}
