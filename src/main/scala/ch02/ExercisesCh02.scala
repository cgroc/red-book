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
}
