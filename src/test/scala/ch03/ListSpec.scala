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

  "List.listLength" should {

    "return 0 for an empty list" in {

      val empty: List[String] = Nil
      listLength(empty) shouldBe 0
    }

    "return 4 for a list of 4 Strings" in {

      val l: List[String] = Cons("Hello", Cons("I", Cons("Like", Cons("Gravy", Nil))))
      listLength(l) shouldBe 4
    }

    "also return the correct length for a list of doubles" in {

      val l: List[Double] = Cons(3.2, Cons(1.7, Cons(4.8, Cons(0.008, Nil))))
      listLength(l) shouldBe 4
    }
  }

  "List.foldLeft" should {

    "return 0 for the sum of an empty list of ints" in {

      val empty: List[Int] = Nil
      foldLeft(empty, 0)(_ + _) shouldBe 0
    }

    "return the correct value for a sum of doubles" in {

      val doubles: List[Double] = Cons(2.0, Cons(3.0, Nil))
      foldLeft(doubles, 1.0)(_ * _) shouldBe 6.0
    }
  }

  "Fold operations" should {

    "Operate on the list elements in the right order" in {

      val doubles: List[Double] = Cons(2.0, Cons(4.0, Nil))
      foldRight(doubles, 1.0)(_ / _) shouldBe 0.5 // (2.0 / 4.0 / 1.0) = 0.5
      foldLeft(doubles, 1.0)(_ / _) shouldBe 0.125 // (1.0 / 4.0 / 2.0) = 0.125
    }
  }

  "FoldRight" should {

    "Operate the same way for each implementation" in {

      val doubles: List[Double] = Cons(2.0, Cons(4.0, Nil))
      foldRight(doubles, 1.0)(_ / _) shouldBe foldRight2(doubles, 1.0)(_ / _)
      foldRight(doubles, 1.0)(_ * _) shouldBe foldRight2(doubles, 1.0)(_ * _)

      val strings: List[String] = Cons("I", Cons("Am", Cons("A", Cons("Mole", Nil))))
      foldRight(strings, "")(_ + _) shouldBe foldRight2(strings, "")(_ + _)
    }
  }

  "List.listLengthLeft" should {

    "return 0 for an empty list" in {

      val empty: List[String] = Nil
      listLengthLeft(empty) shouldBe 0
    }

    "return 4 for a list of 4 Strings" in {

      val l: List[String] = Cons("Hello", Cons("I", Cons("Like", Cons("Gravy", Nil))))
      listLengthLeft(l) shouldBe 4
    }

    "also return the correct length for a list of doubles" in {

      val l: List[Double] = Cons(3.2, Cons(1.7, Cons(4.8, Cons(0.008, Nil))))
      listLengthLeft(l) shouldBe 4
    }
  }

  "List.foldLeft2" should {

    "return 0 for the sum of an empty list of ints" in {

      val empty: List[Int] = Nil
      foldLeft2(empty, 0)(_ + _) shouldBe 0
    }

    "return the correct value for a sum of doubles" in {

      val doubles: List[Double] = Cons(2.0, Cons(3.0, Nil))
      foldLeft2(doubles, 1.0)(_ * _) shouldBe 6.0
    }

    "Operate on the list elements in the right order" in {

      val doubles: List[Double] = Cons(2.0, Cons(4.0, Nil))
      foldLeft2(doubles, 1.0)(_ / _) shouldBe 0.125 // (1.0 / 4.0 / 2.0) = 0.125
    }
  }

  "List.appendLeft" should {

    "Combine two empty lists in to an empty list" in {
      val l1: List[Int] = Nil
      val l2: List[Int] = Nil

      appendLeft(l1, l2) shouldBe Nil
    }

    "Combine a one element list and an empty list in to the one element list" in {
      val l1: List[Int] = Cons(1, Nil)
      val l2: List[Int] = Nil

      appendLeft(l1, l2) shouldBe l1
      appendRight(l1, l2) shouldBe l1
    }

    "Do the same thing the other way around" in {
      val l1: List[Int] = Nil
      val l2: List[Int] = Cons(1, Nil)

      appendLeft(l1, l2) shouldBe l2
      appendRight(l1, l2) shouldBe l2
    }

    "Combine two non-empty lists" in {
      val l1: List[String] = Cons("Hello", Cons("I", Nil))
      val l2: List[String] = Cons("Like", Cons("Scala", Nil))

      appendLeft(l1, l2) shouldBe Cons("Hello", Cons("I", Cons("Like", Cons("Scala", Nil))))
      appendRight(l1, l2) shouldBe Cons("Hello", Cons("I", Cons("Like", Cons("Scala", Nil))))
    }
  }

  "List.concatenateLists" should {

    "Concatenate a list of lists" in {

      val l1: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      val l2: List[Int] = Cons(4, Cons(5, Nil))

      concatenateLists(Cons(l1, Cons(l2, Nil))) shouldBe Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
      concatenateLists2(Cons(l1, Cons(l2, Nil))) shouldBe Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Nil)))))
    }
  }

  "List.filterWithFlatMap" should {

    "Filter stuff" in {
      val ints: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil))))))
      val evens: List[Int] = Cons(2, Cons(4, Cons(6, Nil)))

      filterWithFlatMap(ints)(_ % 2 == 0) shouldBe evens

    }
  }

  "List.addCorrespondingEntries" should {

    "add up some integers" in {
      val first: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      val second: List[Int] = Cons(2, Cons(3, Cons(4, Nil)))
      val expect: List[Int] = Cons(3, Cons(5, Cons(7, Nil)))

      addCorrespondingEntries(first, second) shouldBe expect
    }

  }

  "List.zipWith" should {

    "add up some integers" in {
      val first: List[Int] = Cons(1, Cons(2, Cons(3, Nil)))
      val second: List[Int] = Cons(2, Cons(3, Cons(4, Nil)))
      val expect: List[Int] = Cons(3, Cons(5, Cons(7, Nil)))

      zipWith(first, second)(_ + _) shouldBe expect
    }

  }
}
