package ch02_live

import scala.annotation.tailrec

object LiveCh02 {
  // Exercise 2.1 - Fibbonacci numbers
  def sillyFib(n: Int): Int = {
    require(n >= 0)
    if(n == 0) 0
    else if(n == 1) 1
    else sillyFib(n - 1) + sillyFib(n - 2)
  }

  def fib(n: Int): Int = {
    @tailrec
    def loop(position: Int, latestAcc: Int, previousAcc: Int): Int =
      if(position == 0) previousAcc
      else loop(position - 1, previousAcc + latestAcc, latestAcc)
    loop(n, 1, 0)
  }


  // Exercise 2.2 - isSorted
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean =
    if(as.length <= 1) true
    else ordered(as.head, as.tail.head) && isSorted(as.tail, ordered)

  // Exercise 2.3 - curry
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // Exercise 2.4 - uncurry
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // Exercise 2.5 - compose
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

}
