package ch03

import org.scalatest.{Matchers, WordSpec}
import List._

class ListSpec extends WordSpec with Matchers {

  "List.tail" should {

    "return the tail of a non-empty list" in {
      val ints = Cons(1, Cons(2, Cons(3, Nil)))
      tail(ints) shouldBe Cons(2, Cons(3, Nil))
    }

    "return Nil for a list with one element" in {
      val strings = Cons("Hello", Nil)
      tail(strings) shouldBe Nil
    }

    "return Nil for an empty list" in {
      tail(Nil) shouldBe Nil
    }
  }

  "List.setHead" should {

    "Return a list with one the new head" in {
      val strings = Cons("I", Cons("Like", Cons("Chips", Nil)))
      setHead("We", strings) shouldBe Cons("We", Cons("Like", Cons("Chips", Nil)))
    }
  }

  "List.drop" should {

    "Drop a specified number of elements from a list" in {
      val strings = Cons("I", Cons("Really", Cons("Like", Cons("Chips", Nil))))
      drop(strings, 3) shouldBe Cons("Chips", Nil)
    }
  }

  "List.dropWhile" should {

    "Drop elements from the front of a list while they meet a predicate" in {
      val numbers = Cons(2, Cons(4, Cons(6, Cons(3, Nil))))
      dropWhile(numbers, (n: Int) => n % 2 == 0) shouldBe Cons(3, Nil)
    }
  }

  "List.init" should {

    "Return Nil for an empty list" in {
      val l = Nil
      init(l) shouldBe Nil
    }

    "Return Nil for a list with one element" in {
      val l = Cons("Hello", Nil)
      init(l) shouldBe Nil
    }

    "Return the first three elements for a 4 element list" in {
      val l = Cons("Hello", Cons("I", Cons("Like", Cons("Gravy", Nil))))
      init(l) shouldBe Cons("Hello", Cons("I", Cons("Like", Nil)))
    }
  }

  "List.sum2" should  {

    "return 0 for an empty list" in {

      val l: List[Int] = Nil
      sum2(l) shouldBe 0
    }

    "return the correct value for a list with one int" in {

      val l: List[Int] = Cons(5, Nil)
      sum2(l) shouldBe 5
    }

    "return the correct value for a list with several elements" in {

      val ints = Cons(1, Cons(2, Cons(3, Nil)))
      sum2(ints) shouldBe 6
    }
  }

  "List.product2" should  {

    "return 0 for an empty list" in {

      val l: List[Int] = Nil
      product2(l) shouldBe 1 //TODO: Standard scala foldRight also behaves this way, means you have to be careful with empty sequences?
    }

    "return the correct value for a list with one int" in {

      val l: List[Int] = Cons(5, Nil)
      product2(l) shouldBe 5
    }

    "return the correct value for a list with several elements" in {

      val ints = Cons(1, Cons(2, Cons(4, Nil)))
      product2(ints) shouldBe 8
    }
  }
}
