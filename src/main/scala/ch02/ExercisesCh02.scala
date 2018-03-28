package ch02

import scala.annotation.tailrec

object ExercisesCh02 {

  def factorial(i: Int): Int = {
    @tailrec
    def go(j: Int, acc: Int): Int =
      if(j <= 0) acc
      else go(j - 1, acc * j)

    go(i, 1)
  }

  def fib(i: Int): Int = {
    @tailrec
    def go(j: Int, finalAcc: Int, tempAcc: Int): Int = {
      if (j < 1) finalAcc
      else go(j - 1, tempAcc, finalAcc + tempAcc)
    }

    go(i, 0, 1)
  }

  @tailrec
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    if(as.length <= 1)
      true
    else
      ordered(as.head, as.tail.head) && isSorted(as.tail, ordered)

}
