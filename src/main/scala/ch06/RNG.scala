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

}

object Main extends App {
  println(SimpleRNG(42).nextInt)
  println(SimpleRNG(42).nextInt)

//  println(Int.MaxValue)
//  println(Int.MinValue)

  import ch06.SimpleRNG.Rand

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)



  println(int)
}
