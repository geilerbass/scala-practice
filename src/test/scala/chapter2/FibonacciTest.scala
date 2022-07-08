package chapter2

import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class FibonacciTest extends AnyFunSuite {

  def fibonacci(n: Int): Int = {
    @tailrec
    def getNthValue(i: Int, current: Int, previous: Int): Int = {
      if (i >= n) current
      else getNthValue(i + 1, current + previous, current)
    }
    if (n == 0) 0
    else if (n == 1) 1
    else getNthValue(1, 1, 0)
  }

  def summation(n: Int): Int = {

    def add(prev: Int, x: Int, n: Int): Int = {
      val sum = prev + x
      if (x + 1 > n) sum
      else add(sum, x + 1, n)
    }
    add(0, 0, n)
  }

  test("Fibonacci function returns nth value") {
    assert(fibonacci(0) == 0)
    assert(fibonacci(1) == 1)
    assert(fibonacci(2) == 1)
    assert(fibonacci(3) == 2)
    assert(fibonacci(4) == 3)
    assert(fibonacci(5) == 5)
    assert(fibonacci(6) == 8)
  }
}
