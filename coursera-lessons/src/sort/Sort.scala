package sort

class Sort {

  def insertion(list: List[Int]): List[Int] = {
    list match {
      case List() => List()
      case head :: tail => insert(head, insertion(tail))
    }
  }

  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x < y) x :: y :: ys else y :: insert(x, ys)
  }

}
