package chapter5

import chapter5.Stream.{cons, constant, empty, unfold}
import org.scalatest.funsuite.AnyFunSuite


sealed trait Stream[+A] {
  def scanRight[B](init: B)(f: (A, => B) => B): Stream[B] = {
    foldRight(init -> Stream(init)) { (a, b0) =>
      lazy val b1 = b0
      val b2 = f(a, b1._1)
      (b2, cons(b2, b1._2))
    }._2
  }


//  def tailsWithScanRight: Stream[Stream[A]] = ???
//
//  def hasSubsequence2[A](s: Stream[A]): Boolean = {
//    tailsWithScanRight exists(_ startsWith s)
//  }

  def hasSubsequence[A](s: Stream[A]): Boolean = {
    tails exists (_ startsWith s)
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case s@Cons(_, t) => Some(s, t())
      case Empty => None
    }
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def startsWith[A](s: Stream[A]): Boolean = {
    zipWith(s)((a1,a2) => a1 == a2).foldRight(true)((a1, a2) => a1 && a2)
//    zipAll(s).takeWhile(_._1.isDefined).forAll {case (a1, a2) => a1 == a2}
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Empty) => None
    }
  }

  def zipWith[B,C](that: Stream[B])(f: (A,B) => C): Stream[C] = {
    unfold((this, that)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def takeWhileUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def takeUnfold(i: Int): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if i > 0 =>  Some(h(), t().takeUnfold(i - 1))
      case _ => None
    }
  }

  def mapUnfold[B](f: A => B): Stream[B] = {
    unfold(this){
      case Cons(h, t) => Some(f(h()),t())
      case _ => None
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((a,b) => f(a).append(b))
  }

  def append[A2>:A](f: => Stream[A2]): Stream[A2] = {
    foldRight(f)((a,b) => cons(a, b))
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a,b) => cons(f(a), b))
  }

  def headOption2: Option[A] = {
    foldRight(None: Option[A])((a,_) => Some(a))
  }

  def takeWhile2(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a,b) => p(a) && b)
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(_, t) if (n > 0) => t().drop(n - 1)
      case _ => this
    }
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }
  }

  def toList: List[A] = {
    this match {
      case Empty => List()
      case Cons(h, t) => h() :: t().toList
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  val onesUnfold: Stream[Int] = {
    constantUnfold(1)
  }

  def constantUnfold[A](i: A): Stream[A] = {
    unfold(i)(_ => Some(i, i))
  }

  def fromUnfold(i: Int): Stream[Int] = {
    unfold(i)(x => Some(x, x + 1))
  }

  val fibsUnfold: Stream[Int] = {
    unfold((0, 1)){case (x, y) => Some(x, (y, x + y))}
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case _ => empty
    }
  }


  val fibs: Stream[Int] = {
    def fibAdd(x: => Int, y: => Int): Stream[Int] = {
      cons(x, fibAdd(y, x + y))
    }
    fibAdd(0, 1)
  }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    cons(a, constant(a))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }
}

class PracticeTest extends AnyFunSuite {

  test("Exercise 5.1 ") {
    assert(Stream.empty.toList == List())
    assert(Stream.apply(1,2,3,4,5).toList == List(1,2,3,4,5))
    assert(Stream.apply("This", "is", "a", "lazy", "stream").toList == List("This", "is", "a", "lazy", "stream"))
  }

  test("Exercise 5.2 take and drop") {
    assert(Stream.empty.take(1) == Stream.empty)
    assert(Stream.apply(1).take(1).toList == Stream.apply(1).toList)
    assert(Stream.apply(1,2,3,4,5).take(3).toList == Stream.apply(1,2,3).toList)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").take(2).toList == Stream.apply("This", "is").toList)

    assert(Stream.empty.drop(1) == Stream.empty)
    assert(Stream.apply(1).drop(1) == Stream.empty)
    assert(Stream.apply(1,2,3,4,5).drop(3).toList == Stream.apply(4,5).toList)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").drop(2).toList == Stream.apply("a", "lazy", "stream").toList)
  }

  test("Exercise 5.3 takeWhile") {
    assert(Stream.apply(1,2,3,4,5).takeWhile(_ % 2 != 0).toList == List(1))
    assert(Stream.apply(1,2,3,4,5).takeWhile(_ < 10).toList == List(1,2,3,4,5))
    assert(Stream.apply(1,2,3,4,5).takeWhile(_ > 10) == empty)
    assert(Empty.takeWhile(_ => false) == empty)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").takeWhile(_.length > 3).toList == Stream.apply("This").toList)
  }

  test("Exercise 5.4 forAll") {
    assert(Stream.apply(1, 2, 3, 4, 5).forAll(_ < 6))
    assert(!Stream.apply(1, 2, 3, 4, 5).forAll(_ > 6))
    assert(!Stream.apply(1, 2, 3, 4, 5).forAll(_ > 3))
    assert(Stream.apply("This", "is", "a", "lazy", "stream").forAll(_.length < 7))
    assert(!Stream.apply("This", "is", "a", "lazy", "stream").forAll(_.length > 7))
    assert(!Stream.apply("This", "is", "a", "lazy", "stream").forAll(_.length > 3))
  }

  test("Exercise 5.5 takeWhile folded") {
    assert(Stream.apply(1,2,3,4,5).takeWhile2(_ <= 2).toList == List(1,2))
    assert(Stream.apply(1,2,3,4,5).takeWhile2(_ > 10) == empty)
    assert(Empty.takeWhile2(_ => false) == empty)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").takeWhile2(_.length < 5).toList == Stream.apply("This", "is", "a", "lazy").toList)
  }

  test("Exercise 5.6 headOption with foldRight") {
    assert(Stream.apply(1, 2, 3, 4, 5).headOption2.contains(1))
    assert(Stream.apply(1).headOption2.contains(1))
    assert(Empty.headOption2.isEmpty)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").headOption2.contains("This"))
  }

  test("Exercise 5.7 map with foldRight") {
    assert(Stream.apply(1, 2, 3, 4, 5).map(_ + 1).toList == List(2,3,4,5,6))
    assert(empty[Int].map(_ + 1) == empty)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").map(_.length).toList == List(4,2,1,4,6))
  }

  test("Exercise 5.7 filter with foldRight") {
    assert(Stream.apply(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList == List(2,4))
    assert(empty[Int].filter(_ % 1 == 0) == empty)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").filter(_.length > 3).toList == List("This", "lazy", "stream"))
  }

  test("Exercise 5.7 append with foldRight") {
    assert(Stream.apply(1, 2, 3, 4, 5).append(cons(6, empty)).toList == List(1,2,3,4,5,6))
    assert(empty[Int].append(cons(1, empty)).toList == List(1))
    assert(Stream.apply("This", "is", "a", "lazy", "stream").append(cons("isn't", empty)).append(cons("it", empty)).toList
    == List("This", "is", "a", "lazy", "stream", "isn't", "it"))
  }

  test("Exercise 5.7 flatMap with foldRight") {
    assert(Stream.apply(1,2,3,4,5).flatMap(i => Stream.apply(i,i)).toList == List(1,1,2,2,3,3,4,4,5,5))
    assert(empty[Stream[Int]].flatMap(i => Stream.apply(i,i)) == empty)
  }

  test("Exercise 5.8 constant infinite stream") {
    assert(constant(3).take(5).toList == List(3,3,3,3,3))
    assert(constant("a").take(3).toList == List("a","a","a"))
  }


  test("Exercise 5.9 from returns infinite stream of increasing integers") {
    assert(Stream.from(0).take(5).toList == List(0,1,2,3,4))
    assert(Stream.from(1000).take(3).toList == List(1000,1001,1002))
  }

  test("Exercise 5.10 fibs returns infinite Fibonacci sequence") {
    assert(Stream.fibs.take(12).toList == List(0,1,1,2,3,5,8,13,21,34,55,89))
  }

  test("Exercise 5.11 unfold returns infinite stream") {
    assert(Stream.unfold(0)(x => Some(x, x + 2)).take(4).toList == List(0,2,4,6))
  }

  test("Exercise 5.12 fibsUnfold returns infinite Fibonacci sequence") {
    assert(Stream.fibsUnfold.take(12).toList == List(0,1,1,2,3,5,8,13,21,34,55,89))
  }

  test("Exercise 5.12 fromUnfold returns infinite stream of increasing integers") {
    assert(Stream.fromUnfold(0).take(5).toList == List(0,1,2,3,4))
    assert(Stream.fromUnfold(1000).take(3).toList == List(1000,1001,1002))
  }

  test("Exercise 5.12 constantUnfold returns infinite stream") {
    assert(Stream.constantUnfold(3).take(5).toList == List(3,3,3,3,3))
    assert(Stream.constantUnfold("a").take(3).toList == List("a","a","a"))
  }

  test("Exercise 5.12 onesUnfold returns infinite stream of ones") {
    assert(Stream.onesUnfold.take(5).toList == List(1,1,1,1,1))
  }

  test("Exercise 5.13 implement map with unfold") {
    assert(Stream.apply(1, 2, 3, 4, 5).mapUnfold(_ + 1).toList == List(2,3,4,5,6))
    assert(empty[Int].mapUnfold(_ + 1) == empty)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").mapUnfold(_.length).toList == List(4,2,1,4,6))
  }

  test("Exercise 5.13 implement take with unfold") {
    assert(Stream.empty.takeUnfold(1) == Stream.empty)
    assert(Stream.apply(1).takeUnfold(1).toList == Stream.apply(1).toList)
    assert(Stream.apply(1, 2, 3, 4, 5).takeUnfold(3).toList == Stream.apply(1, 2, 3).toList)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").takeUnfold(2).toList == Stream.apply("This", "is").toList)
  }

  test("Exercise 5.13 implement takeWhile with unfold") {
    assert(Stream.apply(1,2,3,4,5).takeWhileUnfold(_ % 2 != 0).toList == List(1))
    assert(Stream.apply(1,2,3,4,5).takeWhileUnfold(_ < 10).toList == List(1,2,3,4,5))
    assert(Stream.apply(1,2,3,4,5).takeWhileUnfold(_ > 10) == empty)
    assert(Empty.takeWhileUnfold(_ => false) == empty)
    assert(Stream.apply("This", "is", "a", "lazy", "stream").takeWhileUnfold(_.length > 3).toList == Stream.apply("This").toList)
  }

  test("Exercise 5.13 implement zipWith with unfold") {
    assert(Stream.apply(1,2,3,4,5).zipWith(Stream.apply(2,3,4,5,6))((x,y) => x + y).toList == List(3,5,7,9,11))
    assert(Stream.apply(1,2,3,4,5).zipWith(Stream.apply(2,3,4))((x,y) => x + y).toList == List(3,5,7))
  }

  test("Exercise 5.13 implement zipAll with unfold") {
    assert(Stream.apply(1,2,3,4,5).zipAll(Stream.apply(2,3,4,5,6)).toList == List((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), Some(4)), (Some(4), Some(5)), (Some(5), Some(6))))
    assert(Stream.apply(1,2,3,4,5).zipAll(Stream.apply(2,3,4)).toList == List((Some(1), Some(2)), (Some(2), Some(3)), (Some(3), Some(4)), (Some(4), None), (Some(5), None)))
  }

  test("Exercise 5.14 startsWith returns true if stream starts with argument stream") {
    assert(Stream.apply(1,2,3,4,5).startsWith(Stream.apply(1,2,3)))
    assert(!Stream.apply(1,2,3,4,5).startsWith(Stream.apply(3,2,1)))
    assert(Stream.empty.startsWith(Stream.empty))
    assert(!Stream.apply("Some", "text", "here").startsWith(Stream.apply(1,2,3)))
  }

  test("Exercise 5.15 tails returns stream of suffixes") {
    assert(Stream(1,2,3).hasSubsequence(Stream(2,3)))
    assert(!Stream(1,2,3).hasSubsequence(Stream(6)))
    assert(Stream(1,2,3).hasSubsequence(Stream()))
  }

  test("Exercise 5.16 tails using ScanRight") {
    assert(Stream(1,2,3).scanRight(0)(_ + _).toList == List(6,5,3,0))
//    assert(Stream(1,2,3).hasSubsequence2(Stream(2,3)))
//    assert(!Stream(1,2,3).hasSubsequence2(Stream(6)))
//    assert(Stream(1,2,3).hasSubsequence2(Stream()))
  }
}
