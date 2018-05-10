package ch02_live

import LiveCh02._

object App {

  def main(args: Array[String]): Unit = {
    println("Time for some Scala fun!")
//    println(sillyFib(0))
//    println(sillyFib(1))
//    println(sillyFib(2))
//    println(sillyFib(3))
//    println(sillyFib(4))
//    println(sillyFib(5))
//    println(sillyFib(1000))

    val makeAdder: Int => Int => Int = curry[Int, Int, Int]((a, b) => a + b) // Int => (Int => Int)
    val addOne: Int => Int = makeAdder(1) // Int => Int
    println(addOne(7))

    // def compose[A,B,C](f: B => C, g: A => B): A => C =

    val twiceTheLengthOfTheString: String => Int = compose[String, Int, Int](l => l*2, w => w.length)

    val uncurried: (Int, Int) => Int = Function.uncurried(makeAdder)
    println("UNCURRY: " + uncurried(1, 2))

    println(twiceTheLengthOfTheString("scala"))
  }

}
