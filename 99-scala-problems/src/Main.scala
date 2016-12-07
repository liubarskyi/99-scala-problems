
object Main extends App {

  def length[T](list: List[T]): Int = {
    list match {
      case _ :: tail => 1 + length(tail)
      case Nil => 0
    }
  }

  def lengthPureFunctional[T](list: List[T]): Int = {
    list.foldLeft(0)((counter, element) => counter + 1)
  }

  def last[T](list: List[T]): T = {
    list match {
      case something :: Nil => something
      case something :: tail => last(tail)
      case _ => throw new NoSuchElementException
    }
  }

  def preLast[T](list: List[T]): T = {
    list match {
      case pre :: last :: Nil => pre
      case pre :: tail => preLast(tail)
      case _ => throw new NoSuchElementException
    }
  }

  def fromStart[T](k: Int, list: List[T]): T = {
    k match {
      case k if k < 0 | k > list.size - 1 => throw new NoSuchElementException
      case 0 => list.head
      case _ => fromStart(k - 1, list.tail)
    }
  }

  def reverse[T](list: List[T]): List[T] = {
    list match {
      case Nil => Nil
      case first :: tail => reverse(tail) ::: first :: Nil
    }
  }

  def reversePureFunctional[T](list: List[T]): List[T] = {
    list.foldLeft(List[T]())((acc, elem) => elem :: acc)
  }

  def compress[T](list: List[T]): List[T] = {
    list.foldLeft(List[T]())((acc, elem) => if (acc.nonEmpty && acc.last == elem) acc else acc :+ elem)
  }

  def balance(chars: List[Char]): Boolean = {
    chars.map({
      case '(' => 1
      case ')' => -1
      case _ => 0
    }).foldLeft(0)((acc, element) => {
      if (acc < 0) -1
      else acc + element
    }) == 0
  }

  def pack[T](list: List[T]): List[List[T]] = {
    list.foldLeft(List[List[T]]())((acc, elem) => {
      if (acc.nonEmpty && acc.last.head == elem) acc.init :+ (acc.last :+ elem)
      else acc :+ List(elem)
    })
  }

  def encode[T](list: List[T]): List[(Int, T)] = {
    pack(list).map(packed => (packed.size, packed.head))
  }

  def encodeModified[T](list: List[T]): List[Any] = {
    pack(list).map(packed => if (packed.size == 1) packed.head else (packed.size, packed.head))
  }

  def decode[T](encoded: List[(Int, T)]): List[T] = {
    encoded.flatMap(tuple => List.fill(tuple._1)(tuple._2))
  }

  def encodeDirect[T](list: List[T]): List[(Int, T)] = {
    if (list.isEmpty) Nil
    else {
      val (packed, tail) = list.span(_ == list.head)
      (packed.size, packed.head) :: encodeDirect(tail)
    }
  }

  def duplicate[T](list: List[T]): List[T] = {
    list.flatMap(e => List(e, e))
  }

  def duplicateTimes[T](n: Int, list: List[T]): List[T] = {
    list.flatMap(e => List.fill(n)(e))
  }

  def drop[T](n: Int, list: List[T]): List[T] = {
    if (list.isEmpty) Nil
    else {
      if (n == 1) drop(3, list.tail)
      else list.head :: drop(n - 1, list.tail)
    }
  }

  def dropPureFunctional[T](n: Int, list: List[T]): List[T] = {
    list.zipWithIndex.filter(e => (e._2 + 1) % n != 0).map(e => e._1)
  }

  def split[T](n: Int, list: List[T]): (List[T], List[T]) = {
    def recursive(n: Int, first: List[T], second: List[T]): (List[T], List[T]) = {
      if (n == 0) (first, second)
      else recursive(n - 1, first :+ second.head, second.tail)
    }
    if (n < 1 || n > list.size - 1) throw new IllegalArgumentException
    else recursive(n, List(), list)
  }

  def splitPureFunctionalSpan[T](n: Int, list: List[T]): (List[T], List[T]) = {
    if (n < 1 || n > list.size - 1) throw new IllegalArgumentException
    val splitted = list.zipWithIndex.span(tuple => tuple._2 < n)
    (splitted._1.map(e => e._1), splitted._2.map(e => e._1))
  }

  def splitPureFunctionalFold[T](n: Int, list: List[T]): (List[T], List[T]) = {
    if (n < 1 || n > list.size - 1) throw new IllegalArgumentException
    else list.foldLeft((List[T](), List[T]()))((splitted, e) => if (splitted._1.size < n) (splitted._1 :+ e, splitted._2) else (splitted._1, splitted._2 :+ e))
  }

  def slice[T](from: Int, to: Int, list: List[T]): List[T] = {
    if (from < 0 || to > list.size || from > to) throw new IllegalArgumentException
    else dropLats(list.size - to, dropFirst(from, list))
  }

  def dropFirst[T](n: Int, list: List[T]): List[T] = {
    if (n == 0) list
    else dropFirst(n - 1, list.tail)
  }

  def dropLats[T](n: Int, list: List[T]): List[T] = {
    if (n == 0) list
    else dropLats(n - 1, list.init)
  }

  def slicePureFunctional[T](from: Int, to: Int, list: List[T]): List[T] = {
    if (from < 0 || to > list.size || from > to) throw new IllegalArgumentException
    else list.zipWithIndex.filter(t => t._2 >= from && t._2 < to).map(t => t._1)
  }

  def rotateOne[T](n: Int, list: List[T]): List[T] = {
    if (n > 0) rotateOne(n - 1, list.tail :+ list.head)
    else if (n < 0) rotateOne(n + 1, list.last +: list.init)
    else list
  }

  def rotateTwo[T](n: Int, list: List[T]): List[T] = {
    val m = (list.size + n) % list.size // treat positive and negative 'n' the same way
    if (m == 0) list
    else rotateTwo(m - 1, list.tail :+ list.head)
  }

  def rotatePureFunctional[T](n: Int, list: List[T]): List[T] = {
    val m = (list.size + n) % list.size
    list.drop(m) ::: list.take(m)
  }

}
