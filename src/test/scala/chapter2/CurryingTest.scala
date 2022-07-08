package chapter2

import org.scalatest.funsuite.AnyFunSuite

class CurryingTest extends AnyFunSuite{

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def unCurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a).apply(b)
  }

  test("currying") {
    assert(curry((a: Int, b: Int) => a + b).apply(2).apply(3) == 5)
  }

  test("uncurrying") {
    assert(unCurry((a: Int) => (b: Int) => a + b).apply(2, 3) == 5)
  }

  def compose[A, B, C](f: A => B, g: B => C): A => C = {
    a => g.apply(f.apply(a))
  }

  test("compose") {
    assert(compose((a: Int) => a + 2, (b: Int) => b + 3).apply(2) == 7)
  }

}
