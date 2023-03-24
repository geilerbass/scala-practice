package chapter3

import chapter3.List._
import org.scalatest.funsuite.AnyFunSuite

import scala.annotation.tailrec

class DataStructureTests extends AnyFunSuite {


  test("list pattern-matching") {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case null => 101
    }

    assert(x == 3)
  }

  test("tail removes first element") {
    val list = List(1, 2, 3, 4, 5)
    val shorterList = List(2, 3, 4, 5)

    assert(tail(list) == shorterList)
  }

  test("setHead replaces first element") {
    val list = List(1, 2, 3, 4, 5)
    val changedList = List(6, 2, 3, 4, 5)

    assert(setHead(6, list) == changedList)
  }

  test("drop removes first n elements") {
    val list = List(1, 2, 3, 4, 5, 6)
    val shorterList = List(4, 5, 6)

    assert(drop(3, list) == shorterList)
  }

  test("dropWhile removes eligible elements") {
    val list = List(1, 2, 3, 4, 5, 6)
    val shorterList = List(1, 3, 5)

    assert(dropWhile[Int](list, x => (x % 2) == 0) == shorterList)
  }

  test("init removes last element") {
    val list = List(1, 2, 3, 4, 5)
    val newList = List(1, 2, 3, 4)

    assert(init(list) == newList)
    assert(init(List(1)) == Nil)
    assert(init(List("remove", "the", "last", "element")) == List("remove", "the", "last"))
  }

  test("3.8 pass Nil and chapter5.Cons to foldRight") {
    assert(foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)) == List(1,2,3))
  }

  test("3.9 get length of list with foldRight") {
    assert(length(List(1,2,3)) == 3)
    assert(length(Nil:List[Int]) == 0)
    assert(length(List(1)) == 1)
  }

  test("3.10/11 Implement foldLeft as tail recursive (sum, product, length)") {
    assert(foldLeft(List(1,2,3), 0)(_ + _) == 6)
    assert(foldLeft(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldLeft(List(1.0, 2.0, 3.0), 1.0)(_ * _) == 6.0)
    assert(foldLeft(List(1, 2, 3, 4, 5, 6, 7), 0)((x, _) => x + 1) == 7)
  }

  test("3.12 Return reverse of a list") {
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
  }

  test("3.13 write foldLeft using foldRight and vice versa") {
    assert(foldRight2(List(1,2,3), 0)(_ + _) == 6)
    assert(foldRight2(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldRight2(List(1.0, 2.0, 3.0), 1.0)(_ * _) == 6.0)
    assert(foldRight2(List(1, 2, 3, 4, 5, 6, 7), 0)((_, x) => x + 1) == 7)
    assert(foldRight2(List(1,2,3), List[Int]())(Cons(_,_)) == List(1,2,3))

    assert(foldLeft2(List(1,2,3), 0)(_ + _) == 6)
    assert(foldLeft2(Nil: List[Int], 0)(_ + _) == 0)
    assert(foldLeft2(List(1.0, 2.0, 3.0), 1.0)(_ * _) == 6.0)
    assert(foldLeft2(List(1, 2, 3, 4, 5, 6, 7), 0)((x, _) => x + 1) == 7)
    assert(foldLeft2(List(1,2,3), List[Int]())((b,a) => Cons(a, b)) == List(3, 2, 1))
  }

  test("3.14 append adds element") {
    assert(append(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))

    assert(append2(List(1,2,3), List(4,5,6)) == List(1,2,3,4,5,6))
  }

  test("3.15 concatenate lists") {
    assert(List.concat(List(List(1,2,3), List(4,5,6), List(7,8,9))) == List(1,2,3,4,5,6,7,8,9))
  }

  test("3.16 add1 to all elements") {
    assert(add1(List(1,2,3)) == List(2,3,4))
    assert(add1(List()) == List())
  }

  test("3.17 doubleToString") {
    assert(doubleToString(List(1.0, 4.5, 72.343298)) == List("1.0", "4.5", "72.343298"))
    assert(doubleToString(List()) == List())
  }

  test("3.19 filter unless satisfy predicate") {
    assert(filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2, 4, 6))
  }

  test("3.20 flatMap implementation") {
    assert(flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
  }

  test("3.21 filter using flatmap") {
    assert(filterByFlatMap(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) == List(2,4,6))
  }

  test("3.22/3.23 add corresponding values") {
    assert(addElements(List(1,2,3), List(4,5,6)) == List(5,7,9))
  }

  test("3.24 implement hasSubsequence") {
    assert(hasSubsequence(List(1,2,3,4), List(2,3)))
    assert(!hasSubsequence(List(1,2,3,4), List(1,4)))
    assert(hasSubsequence(List(1), List(1)))
    assert(hasSubsequence(List(1), List()))
    assert(hasSubsequence(List(), List()))
    assert(hasSubsequence(List(1,2,3,4), List()))
    assert(!hasSubsequence(List(), List(1,2)))
    assert(!hasSubsequence(List(1,1,1,1), List(2)))
    assert(hasSubsequence(List(1,1,2,2), List(1,2)))
    assert(hasSubsequence(List(1,1,2,2,1,1), List(2,2)))
  }
}

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def sum2(ints: List[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def product2(ds: List[Double]): Double =
    foldRight(ds, 1.0)(_ * _)

  def tail[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  @tailrec
  def drop[A](n: Int, list: List[A]): List[A] = {
    if (n <= 0) list
    else
      list match {
        case Nil => Nil
        case Cons(_, t) => drop(n - 1, t)
      }
  }

  def dropWhile[A](list: List[A], f: A => Boolean): List[A] = {
    def iterate(x: A, t: List[A]) = {
      val bool = !f(x)
      if (bool) Cons(x, dropWhile(t, f)) else dropWhile(t, f)
    }

    list match {
      case Nil => Nil
      case Cons(x, t) => iterate(x, t)
    }
  }

  def setHead[A](newHead: A, list: List[A]): List[A] = {
    Cons(newHead, tail(list))
  }

  def init[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def length[A](as: List[A]) : Int = {
    foldRight(as, 0)((_, y) => y + 1)
  }

  @tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }
  }

  def reverse[A](xs: List[A]): List[A] = {
    foldLeft(xs, List[A]())((b, a) => Cons(a, b))
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b, a) => f(a, b))
  }

  def foldLeft2[A,B](as: List[A], z: B)(f: (B,A) => B): B = {
    foldRight(as, (b:B) => b)((a, g) => b => g(f(b, a)))(z)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)(Cons(_,_))
  }

  def concat[A](lists: List[List[A]]): List[A] = {
    foldLeft(lists, List[A]())(append)
  }

  def add1(list: List[Int]): List[Int] = {
//    foldLeft(list, Nil:List[Int])((b, a) => append(b, List(a + 1)))
    map(list)(_ + 1)
  }

  def doubleToString(list: List[Double]): List[String] = {
//    foldRight(list, Nil:List[String])((a, b) => chapter5.Cons(a.toString, b))
    map(list)(_.toString)
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B])((a, b) => Cons(f(a), b))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A])((a,b) => if (f(a)) Cons(a, b) else b)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = {
//    foldRight(l, Nil:List[B])((a, z) => foldRight(f(a), z)((a, b) => chapter5.Cons(a, b)))
    concat(map(l)(f))
  }

  def filterByFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    flatMap(l)(a => if (f(a)) List(a) else Nil)
  }

  def addElements(l1: List[Int], l2: List[Int]): List[Int] = {
    zipWith(l1, l2)(_ + _)
  }

  def zipWith[A](a1: List[A], a2: List[A])(f: (A,A) => A): List[A] = {
    (a1, a2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case(Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
  }

  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @tailrec
    def startsWith(sup: List[A], sub: List[A]): Boolean = {
      (sup,sub) match {
        case (Nil, _) => false
        case (Cons(h1, _), Cons(h2, Nil)) => h1 == h2
        case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) => startsWith(t1, t2)
        case _ => false
      }
    }

    sup match {
      case Nil => sub == Nil
      case Cons(_, t) => if (startsWith(sup, sub)) true else hasSubsequence(t, sub)
    }
  }
}
