package ch03

import org.scalatest.{Matchers, WordSpec}

class TreeSpec extends WordSpec with Matchers {

  "Tree.size" should {

    "return 1 for a leaf" in {
      val t: Tree[Int] = Leaf(789)
      Tree.size(t) shouldBe 1
    }

    "return 3 for tree with 3 nodes" in {
      val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
      Tree.size(t) shouldBe 3
    }
  }

  "Tree.maximum" should {

    "return the value of a leaf" in {

      val t = Leaf(7)
      Tree.maximum(t) shouldBe 7
    }

    "return the highest value for a more complicated tree" in {

      val t = Branch(Leaf(8), Leaf(20))
      Tree.maximum(t) shouldBe 20
    }
  }

  "Tree.depth" should {

    "return 0 for a single leaf" in {

      val t = Leaf(7)
      Tree.depth(t) shouldBe 0
    }

    "return 2 for a tree with maximum depth 2" in {

      val t = Branch(Leaf(7), Branch(Leaf(8), Leaf(20)))
      Tree.depth(t) shouldBe 2
    }
  }

  "Tree.map" should {

    "multiply a tree of ints by 2" in {

      val t = Branch(Leaf(7), Branch(Leaf(8), Leaf(20)))
      val expected = Branch(Leaf(14), Branch(Leaf(16), Leaf(40)))
      Tree.map(t)(_*2) shouldBe expected
    }

    "return a tree of string lengths" in {

      val t = Branch(Leaf("One"), Branch(Leaf("Two"), Leaf("Three")))
      val expected = Branch(Leaf(3), Branch(Leaf(3), Leaf(5)))
      Tree.map(t)(_.length) shouldBe expected
    }
  }

  "Tree.fold" should {

    "sum all the elements in a tree" in {

      val t = Branch(Leaf(7), Branch(Leaf(8), Leaf(20)))
      Tree.fold[Int, Int](t, 0)(_+_) shouldBe 35
    }

    "concatenate all the strings in a tree" in {

      val t = Branch(Leaf("One"), Branch(Leaf("Two"), Leaf("Three")))
      Tree.fold[String, String](t, "")(_+_) shouldBe "ThreeTwoOne"
    }
  }

}
