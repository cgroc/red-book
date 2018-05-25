package ch04

import org.scalatest.{FlatSpec, Matchers}
import ch04.VarianceOption._

class VarianceOptionSpec extends FlatSpec with Matchers {

  "mean option" should "calculate the mean correctly" in {
    mean(Nil) shouldBe None
    mean(Seq(10, 20)) shouldBe Some(15)
  }

  "variance option" should "calculate the variance correctly" in {
    variance(Nil) shouldBe None
    variance(Seq(10, 20, 3, 55)) shouldBe Some(399.5)
  }

}
