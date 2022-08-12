package chapter6

import org.scalatest.funsuite.AnyFunSuite

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object RNG {
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }
}


class FunctionalStateTest extends AnyFunSuite {

  test("Test RNG") {
    assert(SimpleRNG(1).nextInt._1 == 384748)
    assert(SimpleRNG(2).nextInt._1 == 769497)
    assert(SimpleRNG(1059025964525L).nextInt._1 == -1281479697)
  }

  test("Exercise 6.1 nonNegativeInt returns non-negative Integers only") {
    assert(RNG.nonNegativeInt(SimpleRNG(1))._1 >= 0)
    assert(RNG.nonNegativeInt(SimpleRNG(1059025964525L))._1 >= 0)
  }

  test("Exercise 6.2 double generates random Double between 0 and 1") {
    assert(RNG.double(SimpleRNG(1))._1 > 0)
    assert(RNG.double(SimpleRNG(1))._1 < 1)
    assert(RNG.double(SimpleRNG(1059025964525L))._1 > 0)
    assert(RNG.double(SimpleRNG(1059025964525L))._1 < 1)
  }


}
