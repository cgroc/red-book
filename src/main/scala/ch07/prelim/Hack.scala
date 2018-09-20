package ch07.prelim

object Hack {

  /*
   * Initial exploratory example, we use divide and conquer in order to split the problem in to parts which could be
   * executed in parallel.
   */
  def sum1(ints: IndexedSeq[Int]): Int =
    if (ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (ints1, ints2) = ints.splitAt(ints.length / 2)
      sum1(ints1) + sum1(ints2)
    }

  /*
   * So now we're on the lookout for some datatype that we can use to represent parallel computations. We know that
   * we have to be able to get some kind of result out of it, so it's got to be a higher-kinded / container of some
   * kind.
   */

  type CPar[A] = () => A // ?

  

}
