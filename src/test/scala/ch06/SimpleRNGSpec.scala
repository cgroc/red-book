package ch06

import org.scalatest.{Matchers, WordSpec}
import org.scalatest.prop.PropertyChecks

class SimpleRNGSpec extends WordSpec with PropertyChecks with Matchers {

  "SimpleRNG.nonNegativeInt" should {

    "always return an integer between 0 and Int.MaxValue inclusive" in {

      forAll { seed: Long =>
        val testInt: Int = SimpleRNG.nonNegativeInt(SimpleRNG(seed))._1
        testInt should be >= 0
        testInt should be <= Int.MaxValue
      }
    }
  }

  "SimpleRNG.double" should {

    "always return a double between 0 and 1.0 (not including 1.0)" in {

      forAll { seed: Long =>
        val testDouble: Double = SimpleRNG.double(SimpleRNG(seed))._1
        testDouble should be >= 0.0
        testDouble should be < 1.0
      }
    }
  }
}
