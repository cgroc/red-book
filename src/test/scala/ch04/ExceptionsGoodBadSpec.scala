package ch04

import org.scalatest.{FlatSpec, Matchers}
import ch04.ExceptionsGoodBad._

class ExceptionsGoodBadSpec extends FlatSpec with Matchers {

  "failingFn" should "throw the arithmetic exception when empty" in {
    failingFn(11) shouldBe 43
  }

  "mean" should "not throw the arithmetic exception when not empty" in {
      mean(Seq(1, 2, 3, 4)) shouldBe 2.5
  }

  "mean" should "throw the arithmetic exception when empty" in {
    assertThrows[ArithmeticException] {
      mean(Nil)
    }
  }

  "optionMean" should "not throw the arithmetic exception when not empty" in {
    optionMean(Seq(1, 2, 3, 4)) shouldBe Some(2.5)
  }

  "optionMean" should "throw the arithmetic exception when empty" in {
    optionMean(Nil) shouldBe None
  }

}
