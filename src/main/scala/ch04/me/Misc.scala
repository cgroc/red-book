package ch04.me

import Option.map2

object Misc {

  //Exercise 4.2

  def mean(xs: Seq[Double]): Option[Double] =
    xs match {
      case Nil => None
      case _ => Some(xs.sum / xs.length)
    }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { m =>
      mean(xs map { x =>
        math.pow(x - m, 2)
      })
    }
  }

//  must be possible
  def varianceFor(xs: Seq[Double]): Option[Double] =
    for {
      ???
    } yield ???


  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try {
      age.toInt
    }
    val optTickets: Option[Int] = Try {
      numberOfSpeedingTickets.toInt
    }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  //just make any old function up
  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double =
    ((100 - age) * numberOfSpeedingTickets) * 10.0

  // Exercise 4.4
  def sequence[A](optionAs: List[Option[A]]): Option[List[A]] = {

    def loop(optionList: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = ???

    optionAs match {
      case Nil => ???
      case optHead :: optTail => ???
    }
  }
}
