package ch02_live

import org.scalatest.{FlatSpec, Matchers}
import LiveCh02._

class LiveCh02Spec extends FlatSpec with Matchers {

  "fib" should "give the correct answer for 0" in {
    fib(0) shouldBe 0
  }

  it should "work for 1 as well" in {
    fib(1) shouldBe 1
  }

  it should "work for 2" in {
    fib(2) shouldBe 1
  }

  it should "work for 3" in {
    fib(3) shouldBe 2
  }

  it should "work for 4" in {
    fib(4) shouldBe 3
  }

  "isSorted" should "work for 1 element" in {
    isSorted[Int](Array(1), (a, b) => a < b) shouldBe true
  }

  it should "work for 4 ordered elements" in {
    isSorted[Int](Array(1, 4, 7, 9), (a, b) => a < b) shouldBe true
  }

  it should "give false for unordered elements" in {
    isSorted[Int](Array(1, 11, 7, 9), (a, b) => a < b) shouldBe false
  }

  it should "return true for an empty array" in {
    isSorted[Int](Array.empty, (a, b) => a < b) shouldBe true
  }

}
