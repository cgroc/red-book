package ch04

import ch04.Maybe.Absent
import org.scalatest.{FlatSpec, Matchers}

class MaybeSpec extends FlatSpec with Matchers {

  val maybeValue: Maybe[Int] = Just(3)
  val maybeNotAValue: Maybe[Int] = Absent

  "map" should "apply a simple function correctly" in {
    maybeValue.map(_ * 3) shouldBe Just(9)
    maybeNotAValue.map(_ * 3) shouldBe Absent
  }

  "flatmap" should "map and then flatten correctly" in {
    val func: Int => Maybe[Int] = a => if (a > 0) Just(30) else Absent
    maybeValue.flatMap(func) shouldBe Just(30)
    maybeNotAValue.flatMap(func) shouldBe Absent
  }

  "getOrElse" should "attempt to retrieve the value" in {
    maybeValue.getOrElse(4) shouldBe 3
    maybeNotAValue.getOrElse(4) shouldBe 4
  }

  "orElse" should "attempt to retrieve the value" in {
    maybeValue.orElse(Just(4)) shouldBe Just(3)
    maybeNotAValue.orElse(Just(4)) shouldBe Maybe(4)
  }

  "filter" should "filter the Maybe" in {
    maybeValue.filter(_ > 4) shouldBe Absent
    maybeNotAValue.filter(_ > 4) shouldBe Absent
  }

  "isDefined" should "isDefined show if the value exists" in {
    maybeValue.isDefined shouldBe true
    maybeNotAValue.isDefined shouldBe false
  }

}
