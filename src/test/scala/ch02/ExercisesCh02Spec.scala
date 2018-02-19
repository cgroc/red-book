package ch02

import org.scalatest.{FlatSpec, Matchers}

class ExercisesCh02Spec extends FlatSpec with Matchers {

  "Factorial" should "return the correct value for 5" in {
    ExercisesCh02.factorial(5) shouldBe 120
  }

  it should "return the correct value for 0" in {
    ExercisesCh02.factorial(0) shouldBe 1
  }

}
