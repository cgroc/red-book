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

}
