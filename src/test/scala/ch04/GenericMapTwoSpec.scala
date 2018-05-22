package ch04

import org.scalatest.{FlatSpec, Matchers}
import ch04.GenericMapTwo._

class GenericMapTwoSpec extends FlatSpec with Matchers {

  "map2" should "take to options and apply the function correctly" in {
    map2(Some(3), Some(2))((a: Int, b: Int) => a * b) shouldBe Some(6)
    map2(Some(3), None)((a: Int, b: Int) => a * b) shouldBe None
    map2(None, Some(2))((a: Int, b: Int) => a * b) shouldBe None
    map2(None, None)((a: Int, b: Int) => a * b) shouldBe None
  }

}
