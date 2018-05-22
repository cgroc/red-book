package ch04

import org.scalatest.{FlatSpec, Matchers}
import ch04.GenericMapTwo._
import ch04.Maybe.Absent

class GenericMapTwoSpec extends FlatSpec with Matchers {

  "map2" should "take to options and apply the function correctly" in {
    map2(Some(3), Some(2))((a: Int, b: Int) => a * b) shouldBe Some(6)
    map2(Some(3), None)((a: Int, b: Int) => a * b) shouldBe None
    map2(None, Some(2))((a: Int, b: Int) => a * b) shouldBe None
    map2(None, None)((a: Int, b: Int) => a * b) shouldBe None
  }

  "sequence" should "transform the list correctly" in {
    sequence(List(Some("1"), Some("2"), Some("s"))) shouldBe Some(List("1", "2", "s"))
    sequence(List(Some("1"), Some("2"), Some("3"))) shouldBe Some(List("1", "2", "3"))
  }

  "parse ints" should "transform the list correctly" in {
    parseInts(List("1", "2", "s")) shouldBe None
    parseInts(List("1", "2", "3")) shouldBe Some(List(1, 2, 3))
  }

  "traverse" should "" in {
    traverse(List(1, 2, 3))((a: Int) => if (a > 10) Just(a * 3) else Absent) shouldBe Absent
    traverse(List(1, 2, 3))((a: Int) => if (a > 0) Just(a * 3) else Absent) shouldBe Just(List(3, 6, 9))
  }

}
