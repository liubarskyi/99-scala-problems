package cacke

object Test extends App {

  trait GreetingProvider {
    def getGreeting: String
  }

  trait RussianGreetingProvider extends GreetingProvider {
    override def getGreeting: String = "privet"
  }

  trait EnglishGreetingProvider extends GreetingProvider {
    override def getGreeting: String = "hello"
  }

  trait Greeter {

    this: GreetingProvider =>

    def greet() = println(getGreeting)
  }

  class DefaultGreeter extends Greeter with EnglishGreetingProvider

  val russianGreeter = new Greeter with RussianGreetingProvider
  val englishGreeter = new Greeter with EnglishGreetingProvider

  russianGreeter.greet()
  englishGreeter.greet()

}
