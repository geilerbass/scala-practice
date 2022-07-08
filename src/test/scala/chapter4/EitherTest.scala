package chapter4

import chapter4.Either.{sequence, traverse}
import org.scalatest.funsuite.AnyFunSuite

class EitherTest extends AnyFunSuite {

  test("4.6 Test Map for Some and None") {
    assert(Right(2).map(_ + 2) == Right(4))
    val value: Either[String, Int] = Left("No value!")
    assert(value.map(_ + 2) == Left("No value!"))
  }

  test("4.6 Test flatMap") {
    assert(Right(2).flatMap(x => Right(x)) == Right(2))
    assert(Right(2).flatMap(x => if (x <= 2) Right(x) else Left("Not in range")) == Right(2))
    assert(Right(2).flatMap(x => if (x > 2) Right(x) else Left("Not in range")) == Left("Not in range"))
    val value: Either[String, String] = Left("No value")
    assert(value.flatMap(x => Right(x)) == Left("No value"))
  }

  test("4.6 Test orElse") {
    assert(Right(2).orElse(Right(3)) == Right(2))
    assert(Left("No value").orElse(Right(3)) == Right(3))
  }

  test("4.6 Test map2") {
    assert(Right(1).map2(Right(2))(_ + _) == Right(3))
    assert(Right(1).map2(Left(""))(_ + _) == Left(""))
    val testValue: Either[String, Int] = Left("")
    assert(testValue.map2(Left(""))(_ + _) == Left(""))
  }

  test("4.7 Traverse and Sequence") {
    assert(traverse(List(1,2,3))(i => Right(i + 1)) == Right(List(2,3,4)))
    assert(sequence(List(Right(1),Right(2),Right(3))) == Right(List(1,2,3)))
    assert(sequence(List(Right(1),Left(""))) == Left(""))
    assert(sequence(List(Left(""),Left(""))) == Left(""))
  }
}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
  }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this.map(f(_)) match {
      case Right(b) => b
      case Left(e) => Left(e)
    }
  }
  def orElse[EE >: E, AA >: A](b: => Either[EE, AA]): Either[EE, AA] = {
    this match {
      case Right(a) => Right(a)
      case _ => b
    }
  }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(a => a)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }
  }
}


