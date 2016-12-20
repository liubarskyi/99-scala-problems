package option

object Test extends App {

  case class People(name: String, first: Option[People], second: Option[People])

  val petya = People("petya", None, None)
  val toma = People("toma", None, None)
  val alla = People("alla", Some(petya), Some(toma))

  val nadya = People("nadya", None, None)
  val vitya = People("vitya", None, Some(nadya))

  val katya = People("katya", Some(vitya), Some(alla))
  val lesha = People("lesha", None, None)

  val milya = People("milya", Some(lesha), Some(katya))

  def parentOf(p: People): Option[People] = p.first

  println(milya.second.flatMap(parentOf).flatMap(parentOf).flatMap(parentOf))

}
