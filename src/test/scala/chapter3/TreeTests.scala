package chapter3

import chapter3.Tree.{depth, depth2, map, map2, maximum, maximum2, size, size2}
import org.scalatest.funsuite.AnyFunSuite

class TreeTests extends AnyFunSuite{
  test("3.25 tree size") {
    assert(size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(size(Leaf(1)) == 1)
    assert(size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 7)
  }

  test("3.26 tree maximum") {
    assert(maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4)
    assert(maximum(Leaf(1)) == 1)
    assert(maximum(Branch(Leaf(1), Leaf(1))) == 1)
    assert(maximum(Branch(Leaf(1), Leaf(2))) == 2)
  }

  test("3.27 tree depth") {
    assert(depth(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 2)
    assert(depth(Leaf(1)) == 0)
    assert(depth(Branch(Leaf(1), Leaf(1))) == 1)
    assert(depth(Branch(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))), Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2))))) == 3)
  }

  test("3.27 tree map") {
    assert(map(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 2) ==
      Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
  }

  test("3.28 fold") {
    assert(size2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 7)
    assert(maximum2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 4)
    assert(depth2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 2)
    assert(map2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4))))(_ + 2) ==
      Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
  }
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => maximum(l) max maximum(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l) max depth(r)
    }
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }
  def size2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l,r) => 1 + l + r)
  }

  def maximum2(t: Tree[Int]): Int = {
    fold(t)(v => v)((l,r) => l max r)
  }

  def depth2[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((l,r) => 1 + l max r)
  }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(v => Leaf(f(v)): Tree[B])(Branch(_,_))
  }
}
