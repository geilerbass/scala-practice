package chapter6

import chapter6.State.unit
import org.scalatest.funsuite.AnyFunSuite

class StandAloneStateTest extends AnyFunSuite {

  test("Test RNG using generic state") {
    assert(NewRNG.nextInt.run(1)._2 == 384748)
//    assert(NewRNG.nextInt.run(5)._2 == 769497)
    assert(NewRNG.nextInt.run(1059025964525L)._2 == -1281479697)

    assert(NewRNG.nonNegativeInt.run(1)._2 >= 0)
//    assert(NewRNG.nonNegativeInt.run(1059025964525L)._2 >= 0)

    assert(NewRNG.intsWithSequence(3).run(1)._2 == List(384748, -1151252339, -549383847))
  }

}



object NewRNG {
  def nextInt: State[Long, Int] = State(seed => {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFL
    val n = (newSeed >>> 16).toInt
    (newSeed, n)
  })

  def nonNegativeInt: State[Long, Int] = {
    nextInt.map(
      i => {
        if (i < 0) -(i + 1)
        else i
      }
    )
  }

  def intsWithSequence(n: Int): State[Long, List[Int]] = {
    sequence(List.fill(n)(nextInt))
  }

  def sequence(fs: List[State[Long, Int]]): State[Long, List[Int]] = {
    fs.foldRight(unit(Nil: List[Int]))((s, acc) => s.map2(acc)(_ :: _))
  }


//  def nonNegativeInt: State[Long, Int] =
}


case class State[S,A](run: S => (S,A)) {
  def map[B](f: A => B): State[S, B] = {
    flatMap(a => unit(f(a)))
  }
  def map2[B,C](sb: State[S,B])(f: (A,B) => C): State[S,C] = {
    flatMap(a => sb.map(b => f(a,b)))
  }

  def flatMap[B](f: A => State[S,B]): State[S,B] =
    State(
      s => {
        val (s1, a) = run(s)
        f(a).run(s1)
      }
    )
}


object State {
  def unit[S,A](a: A): State[S,A] = State(s => (s,a))
}
