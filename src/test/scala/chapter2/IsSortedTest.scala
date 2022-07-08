package chapter2

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class IsSortedTest extends AnyFunSuite{

  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def checkIsSorted(i : Int): Boolean = {
      if (i + 1 >= array.length) true
      else if (!ordered.apply(array(i), array(i + 1))) false
      else checkIsSorted(i + 1)
    }
    checkIsSorted(0)
  }

  test("Is sorted returns correctly") {
    assert(isSorted(Array(1, 2, 3, 4, 5, 6), (a: Int, b: Int) => a < b))
  }

  test("Is not sorted correctly start") {
    assert(!isSorted(Array(1, 3, 2, 4, 5, 6), (a: Int, b: Int) => a < b))
  }

  test("Is not sorted correctly end") {
    assert(!isSorted(Array(1, 2, 3, 4, 6, 5), (a: Int, b: Int) => a < b))
  }
}
