package ch04

object ExceptionsGoodBad {

  case class FirstFailure(message: String) extends Exception

  def failingFn(i: Int): Int = {
    try {
      val x: Int = if (i > 10) throw new ArithmeticException("fail") else 10
      x + 1
    } catch {
      case _: Exception => 43
    }
  }

  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty) throw new ArithmeticException("mean of empty list!")
    else xs.sum / xs.length

  def optionMean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)


}
