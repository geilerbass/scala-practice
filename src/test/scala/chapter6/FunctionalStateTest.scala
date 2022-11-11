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

  def ints(count: Int)(rng: RNG):(List[Int], RNG) = {
    if (count <= 0) {
      (List(), rng)
    } else {
      val (i, r) = nonNegativeInt(rng)
      val (l, r1) = ints(count - 1)(r)
      (i :: l, r1)
    }

  }

  def double3(r: RNG):((Double, Double, Double), RNG) = {
    val (d1, r1) = double(r)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }
  def doubleInt(r: RNG): ((Double, Int), RNG) = {
    intDouble(r) match {
      case ((i, d), r1) => ((d,i), r1)
    }
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = nonNegativeInt(rng)
    val (d, r2) = double(r1)
    ((i,d), r2)
  }


  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B] (s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  val doubleWithMap: Rand[Double] = {
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldRight(unit(Nil: List[A]))((r, acc) => map2(r, acc)(_ :: _))
  }

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  def intsWithSequence(n: Int): Rand[List[Int]] = {
    sequence(List.fill(n)(int))
  }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = {
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        unit(mod)
      else nonNegativeLessThan(n)
    }
  }

  def mapWithFlatMap[A,B](a: Rand[A])(f: A => B): Rand[B] = {
    flatMap(a)(i => unit(f(i)))
  }

  val doubleWithMapViaFlatMap: Rand[Double] = {
    mapWithFlatMap(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))
  }

  def map2WithFlatMap[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
    flatMap(ra) {
      a => flatMap(rb) {
        b => unit(f(a, b))
      }
    }
  }

  val randIntDoubleViaFlatMap: Rand[(Int, Double)] = {
    map2WithFlatMap(int, double)((_, _))
  }

  val randDoubleIntViaFlatMap: Rand[(Double, Int)] = {
    map2WithFlatMap(double, int)((_, _))
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
    assert(RNG.nonNegativeInt(RNG.nonNegativeInt(SimpleRNG(1))._2)._1 == 1151252338)
  }

  test("Exercise 6.2 double generates random Double between 0 and 1") {
    assert(RNG.double(SimpleRNG(1))._1 > 0)
    assert(RNG.double(SimpleRNG(1))._1 < 1)
    assert(RNG.double(SimpleRNG(1059025964525L))._1 > 0)
    assert(RNG.double(SimpleRNG(1059025964525L))._1 < 1)
  }

  test("Exercise 6.3 intDouble returns Int, Double pair") {
    assert(RNG.intDouble(SimpleRNG(1))._1 == (384748, 0.5360936457291245))
  }

  test("Exercise 6.3 doubleInt returns Double, Int pair") {
    assert(RNG.doubleInt(SimpleRNG(1))._1 == (0.5360936457291245, 384748))
  }

  test("Exercise 6.3 double3 returns Double triplet") {
    assert(RNG.double3(SimpleRNG(1))._1 == (0.000179162248969078060, 0.5360936457291245, 0.2558267889544368))
  }

  test("Exercise 6.4 ints generates a list of random integers") {
    assert(RNG.ints(3)(SimpleRNG(1))._1 == List(384748, 1151252338, 549383846))
  }

  test("Exercise 6.5 reimplement double with Map") {
    assert(RNG.doubleWithMap(SimpleRNG(1))._1 == 0.000179162248969078060)
    assert(RNG.doubleWithMap(SimpleRNG(1))._1 < 1)
    assert(RNG.doubleWithMap(SimpleRNG(1059025964525L))._1 > 0)
    assert(RNG.doubleWithMap(SimpleRNG(1059025964525L))._1 < 1)
  }

  test("Exercise 6.6 Implement intDouble and doubleInt with map2") {
    assert(RNG.randIntDouble(SimpleRNG(1))._1 == (384748, 0.5360936457291245))
    assert(RNG.randDoubleInt(SimpleRNG(1))._1 == (0.000179162248969078060, -1151252339))
  }

  test("Exercise 6.7 Implement sequence for combining a list of transitions") {
    assert(RNG.intsWithSequence(3)(SimpleRNG(1))._1 == List(384748, -1151252339, -549383847))
  }

  test("Exercise 6.8 nonNegativeLessThan implemented using flatmap") {
    assert(RNG.nonNegativeLessThan(5)(SimpleRNG(1))._1 == 3)
  }

  test("Exercise 6.9 reimplement map in terms of flatMap") {
    assert(RNG.doubleWithMapViaFlatMap(SimpleRNG(1))._1 == 0.000179162248969078060)
    assert(RNG.doubleWithMapViaFlatMap(SimpleRNG(1))._1 < 1)
    assert(RNG.doubleWithMapViaFlatMap(SimpleRNG(1059025964525L))._1 > 0)
    assert(RNG.doubleWithMapViaFlatMap(SimpleRNG(1059025964525L))._1 < 1)
  }

  test("Exercise 6.9 reimplement intDouble and doubleInt with map2 in terms of flatMap") {
    assert(RNG.randIntDoubleViaFlatMap(SimpleRNG(1))._1 == (384748, 0.5360936457291245))
    assert(RNG.randDoubleIntViaFlatMap(SimpleRNG(1))._1 == (0.000179162248969078060, -1151252339))
  }
}
