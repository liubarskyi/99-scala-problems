package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == Math.min(a, b)
  }

  property("empty after insert and delete") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("sorted") = forAll { h: H =>
    isSorted(toList(h))
  }

  property("melding is sorted") = forAll { (h1: H, h2: H) =>
    isSorted(toList(meld(h1, h2)))
  }

  property("min of melding") = forAll { (h1: H, h2: H) =>
    val min = Math.min(findMin(h1), findMin(h2))
    val h = meld(h1, h2)
    findMin(h) == min
  }

  property("empty") = forAll { (h: H) =>
    isEmpty(empty)
  }

  property("not empty") = forAll { (h: H, i: Int) =>
    !isEmpty(insert(i, h))
  }

  property("not empty") = forAll { (i: Int) =>
    !isEmpty(insert(i, empty))
  }

  property("reinsert min") = forAll { h: H =>
    findMin(insert(findMin(h), deleteMin(h))) == findMin(h)
  }

  property("order does not matter") = forAll { (a: Int, b: Int, c: Int) =>
    toList(from(a, b, c)) == toList(from(a, b, c))
  }

  def toList(h: H): List[A] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: toList(deleteMin(h))
  }

  def isSorted(list: List[A]): Boolean = list match {
    case Nil => true
    case last :: Nil => true
    case first :: second :: tail => if (first > second) false else isSorted(tail)
  }

  def from(list: A*): H = from(scala.util.Random.shuffle(list.toList), empty)

  def from(list: List[A], acc: H): H = list match {
    case Nil => acc
    case head :: tail => from(tail, insert(head, acc))
  }

}
