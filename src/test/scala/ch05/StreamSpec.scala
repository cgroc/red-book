package ch05

import org.scalatest.{Matchers, WordSpec}

class StreamSpec extends WordSpec with Matchers {

  "Stream" when {

    "taking 2 from an empty Stream" should {

      "return an empty stream" in {

        Stream.apply().take(2) shouldBe Stream.empty
      }

    }


    "taking 1 from a Stream of 1 element" should {

      "return a Stream of 1 element" in {

        Stream.apply(1).take(1).toList shouldBe Stream.apply(1).toList
      }
    }

    "calling takeWhile from an Empty Stream" should {

      "return an empty Stream" in {

        Stream.empty[Int].unfoldTakeWhile(_ => true).toList shouldBe Stream.empty[Int].toList
      }
    }

    "calling takeWhile for integers less than 10" should {

      "return a stream of 0 to 9" in {

        Stream.from(0).unfoldTakeWhile(_ < 10).toList shouldBe Stream.apply(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).toList
      }
    }

    "calling Stream.map" should {

      "return an empty Stream mapping over an empty Stream" in {

        Stream.empty[String].unfoldMap(_.length).toList shouldBe Stream.empty[Int].toList
      }

      "return a Streams of lengths when mapped over a Stream of strings" in {

        Stream.apply("one", "blah", "bibble").map(_.length).toList shouldBe Stream.apply(3, 4, 6).toList
      }

    }

    "calling Stream.zipWith" should {

      "work for integer addition" in {

        Stream.apply(1, 2, 3).zipWith(Stream.apply(3, 2, 1))(_ + _).toList shouldBe 4 :: 4 :: 4 :: Nil
      }

      "work where the streams are different lengths" in {

        Stream.apply(4, 5, 6, 7).zipWith(Stream.apply(1, 2, 3))(_ + _).toList shouldBe List(5, 7, 9)
      }

      "work where the first stream is empty" in {

        Stream.empty[Int].zipWith(Stream.apply(1, 2, 3))(_ + _).toList shouldBe List.empty
      }
    }

    "calling ZipAll" should {

      "work for two empty streams" in {
        Stream.empty[Int].zipAll(Stream.empty[Int]).toList shouldBe List.empty
      }

      "work for two streams of the same length" in {
        Stream.apply(1, 2, 3, 4).zipAll(Stream.apply(4, 3, 2, 1)).toList shouldBe List((Some(1), Some(4)), (Some(2), Some(3)), (Some(3), Some(2)), (Some(4), Some(1)))
      }

      "work where the first stream is longer than the second" in {
        Stream.apply(1, 2, 3, 4).zipAll(Stream.apply(4, 3, 2)).toList shouldBe List((Some(1), Some(4)), (Some(2), Some(3)), (Some(3), Some(2)), (Some(4), None))
      }

      "work where the second stream is longer than the first" in {
        Stream.apply(1, 2).zipAll(Stream.apply(4, 3, 2, 1)).toList shouldBe List((Some(1), Some(4)), (Some(2), Some(3)), (None, Some(2)), (None, Some(1)))
      }
    }

    "calling startsWith" should {

      "return true when empty Stream startsWith an empty Stream" in {
        Stream.empty[Int].startsWith(Stream.empty[Int]) shouldBe true
      }

      "return true when a nonEmpty Stream startsWith an empty Stream" in {
        Stream.apply(1, 2, 3).startsWith(Stream.empty[Int]) shouldBe true
      }

      "return true when one Stream starts with another" in {
        Stream.apply(1, 2, 3).startsWith(Stream.apply(1, 2)) shouldBe true
      }

      "return false when one Stream doesn't start with the other" in {
        Stream.apply(1, 2, 3).startsWith(Stream.apply(2, 1)) shouldBe false
      }

      "return false when the Stream being searched for is longer than this one" in {
        Stream.apply(1, 2, 3).startsWith(Stream.apply(1, 2, 3, 4)) shouldBe false
      }
    }

  }



}
