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

  "isSorted" should "return true for an empty array" in {
    ExercisesCh02.isSorted[Int](Array(), (a, b) => a < b) shouldBe true
  }

  it should "return true for an array of one Int" in {
    ExercisesCh02.isSorted[Int](Array(5), (a, b) => a < b) shouldBe true
  }

  it should "return true for an array of one String" in {
    ExercisesCh02.isSorted[String](Array("Hello"), (a, b) => a.length < b.length)
  }

  it should "return false for an unordered array of several elements" in {
    ExercisesCh02.isSorted[Int](Array(1, 2, 3, 4 ,3, 2, 1), (a, b) => a < b) shouldBe false
  }

  it should "return true for an ordered array of several elements" in {
    ExercisesCh02.isSorted[Int](Array(2, 4, 6, 8, 10), (a, b) => a < b) shouldBe true
  }

  it should "also work for simple alphabetical ordering on Strings" in {
    ExercisesCh02.isSorted[String](Array("apple", "banana", "kumkwat", "pear"), (a, b) => a.head < b.head ) shouldBe true
  }
}
