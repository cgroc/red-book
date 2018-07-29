package ch06

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object SimpleRNG {

  // Exercise 6.1
  // Note that this has the form RNG => (Int, RNG), I think I prefer it for the signature to
  // actually look like this:
  // def nonNegativeInt: RNG => (Int, RNG), or even Rand[Int] (but I actually find the latter confising too :'(
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    if (nextInt == Int.MinValue)
      nonNegativeInt(nextRng)
    else
      (Math.abs(nextInt), nextRng)
  }

  // Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRng) = nonNegativeInt(rng)
    if(nextInt == Int.MaxValue)
      double(nextRng)
    else
      (nextInt.toDouble / Int.MaxValue.toDouble, nextRng)
  }

  // Exercise 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextInt, nextRngInt) = rng.nextInt
    val (nextDouble, nextRngDouble) = double(nextRngInt)
    ((nextInt, nextDouble), nextRngDouble)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((nextInt, nextDouble), nextRng) = intDouble(rng)
    ((nextDouble, nextInt), nextRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (double1, rng1) = double(rng)
    val (double2, rng2) = double(rng1)
    val (double3, rng3) = double(rng2)
    ((double1, double2, double3), rng3)
  }

  // Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def loop(currentCount: Int, currentRng: RNG, acc: List[Int]): (List[Int], RNG) =
      if(currentCount <= 0)
        (acc, currentRng)
      else {
        val (nextInt, nextRng) = currentRng.nextInt
        loop(currentCount - 1, nextRng, acc :+ nextInt)
      }

    loop(count, rng, List.empty)
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // Exercise 6.5
  def doubleMap(rng: RNG): (Double, RNG) =
    map[Int, Double](nonNegativeInt) {
      _ / (Int.MaxValue.toDouble + 1) // note algo taken from implementation in fpscala repo
    } (rng)

  // Exercise 6.6
  def map2[A, B, C](s1: Rand[A], s2: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rnga) = s1(rng)
    val (b, rngb) = s2(rnga)
    (f(a, b), rngb)
  }

  // naive implementation before reading on
  def doubleIntMap2(rng: RNG): ((Double, Int), RNG) =
    map2[Double, Int, (Double, Int)](double, int) {
      (i, d) => Tuple2(i, d) // otherwise the syntax is confusing imo
    } (rng)

  // now by the book
  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  // Exercise 6.7
  // fs: List[Rand[A]] a.k.a List[RNG => (A, RNG)]
  // Rand[List[A]] a.k.a RNG => (List[A], RNG)
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs.foldRight[Rand[List[A]]](unit(List.empty)) {
      (randa, randlista) => map2(randa, randlista) {
        (a, lista) => lista.+:(a)
      }
    } (rng)

  def intsSequence(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence[Int](List.fill(count)(int))(rng)

  // Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rnga): (A, RNG) = f(rng)
      g(a)(rnga)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap[Int, Int](nonNegativeInt) {
      i => { // this will have signature Int => Int => (RNG, Int)
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }
    }

  // Exercise 6.9

}

object Main extends App {
  println(SimpleRNG(42).nextInt)
  println(SimpleRNG(42).nextInt)

  import ch06.SimpleRNG._
  println(int(SimpleRNG(42)))/**/
  //  println(Int.MaxValue)
  //  println(Int.MinValue)
  println()
  println(double(SimpleRNG(42)))
  println(doubleMap(SimpleRNG(42))) //slighty different as implementations use slightly different algorithms
  println()
  println(doubleInt(SimpleRNG(42)))
  println(doubleIntMap2(SimpleRNG(42)))
  println()
  println(ints(5)(SimpleRNG(42)))
  println(intsSequence(5)(SimpleRNG(42)))
}
