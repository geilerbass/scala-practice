package chapter4

import chapter4.Option._
import org.scalatest.funsuite.AnyFunSuite

class OptionTest extends AnyFunSuite {
  test("4.1 Test Map for Some and None") {
    assert(Some(2).map(_ + 2) == Some(4))
    val none: Option[Int] = None
    assert(none.map(_ + 2) == None)
  }

  test("4.1 Test flatMap") {
    assert(Some(2).flatMap(x => Some(x)) == Some(2))
    assert(Some(2).flatMap(x => if (x <= 2) Some(x) else None) == Some(2))
    assert(Some(2).flatMap(x => if (x > 2) Some(x) else None) == None)
    assert(None.flatMap(x => Some(x)) == None)
  }

  test("4.1 Test getOrElse") {
    assert(Some(2).getOrElse(3) == 2)
    assert(None.getOrElse(3) == 3)
  }

  test("4.1 Test orElse") {
    assert(Some(2).orElse(Some(3)) == Some(2))
    assert(None.orElse(Some(3)) == Some(3))
  }

  test("4.2 Test Variance") {
    assert(variance(Seq(1,2,3)) == Some(2.0/3.0))
    assert(variance(Seq(1,2,3,4,5,6)) == Some(17.5/ 6.0))
    // 2.5, 1.5, 0.5, 0.5, 1.5, 2.5
    // 6.25, 2.25, 0.25, 0.25, 2.25, 6.25
    assert(variance(Seq()) == None)
   }

  test("4.3 Test map2") {
    assert(map2(Some(1),Some(2))(_ + _) == Some(3))
    assert(map2(Some(1),None.asInstanceOf[Option[Int]])(_ + _) == None)
    assert(map2[Int,Int,Int](None,None)(_ + _) == None)
  }

  test("Test map3") {
    assert(map3(Some(1),Some(2),Some(3))(_ + _ + _) == Some(6))
    assert(map3(Some(1),Some(2),None.asInstanceOf[Option[Int]])(_ + _ + _) == None)
    assert(map3[Int,Int,Int,Int](None,None,None)(_ + _ + _) == None)
  }

  test("4.4 Sequence") {
    assert(traverse(List(1,2,3))(i => Some(i + 1)) == Some(List(2,3,4)))
    assert(sequence(List(Some(1),Some(2),Some(3))) == Some(List(1,2,3)))
    assert(sequence(List(Some(1),None)) == None)
    assert(sequence(List(None,None)) == None)
  }

  test("4.5 Traverse") {
    assert(traverse(List(1,2,3))(i => Some(i + 1)) == Some(List(2,3,4)))
    assert(sequenceByTraverse(List(Some(1),Some(2),Some(3))) == Some(List(1,2,3)))
    assert(sequenceByTraverse(List(Some(1),None)) == None)
    assert(sequenceByTraverse(List(None,None)) == None)
  }
}

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(a) => Some(f(a))
      case None => None
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this map f getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(a) => a
      case None => default
    }
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a flatMap (a => b map (f(a, _)))
  }

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = {
    a flatMap(a => b flatMap(b =>c map (f(a,b,_))))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h flatMap(h => sequence(t)  map (h :: _))
    }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }

  def sequenceByTraverse[A](a: List[Option[A]]): Option[List[A]] = {
    traverse(a)(a => a)
  }
}

