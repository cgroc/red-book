package ch02

import org.scalatest.{FlatSpec, Matchers}

class ExercisesCh02Spec extends FlatSpec with Matchers {

  "Factorial" should "return the correct value for 5" in {
    ExercisesCh02.factorial(5) shouldBe 120
  }

  it should "return the correct value for 0" in {
    ExercisesCh02.factorial(0) shouldBe 1
  }

  "Fibonacci" should "return 0 for non natural numbers" in {
    ExercisesCh02.fib(0) shouldBe 0
    ExercisesCh02.fib(-1) shouldBe 0
  }

  it should "return the correct value for the first three Fibbonacci numbers" in {
    ExercisesCh02.fib(1) shouldBe 1
    ExercisesCh02.fib(2) shouldBe 1
    ExercisesCh02.fib(3) shouldBe 2
  }

  it should "return the correct value for the 20th Fibonacci number" in {
    ExercisesCh02.fib(20) shouldBe 6765
  }

  it should "return 0 when called with numbers less than 1" in {
    ExercisesCh02.fib(0) shouldBe 0
    ExercisesCh02.fib(-23) shouldBe 0
  }
}
