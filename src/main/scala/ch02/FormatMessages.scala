package ch02

import ExercisesCh02.{factorial, fib}

object FormatMessages {

  def formatFact(n: Int): String = {
    val msg = "The factorial of %d is %d"
    msg.format(n, factorial(n))
  }

  def formatFib(n: Int): String = {
    val msg = "The fib of %d is %d"
    msg.format(n, fib(n))
  }

  def format(s: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d"
    msg.format(s, n, f(n))
  }

  def printResultsWithoutHOF(): Unit = {
    println("=== Without HOF ===")
    println(formatFact(5))
    println(formatFib(5))
  }

  def printResultsWithHOF(): Unit = {
    println("=== With HOF ===")
    println(format("factorial", 5, factorial))
    println(format("fib", 5, fib))
  }

  def main(args: Array[String]): Unit = {
    printResultsWithoutHOF()
    printResultsWithHOF()
  }
}
