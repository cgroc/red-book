package ch03

import org.scalatest.{Matchers, WordSpec}

class BinaryTreeSpec extends WordSpec with Matchers {

  "Tree.size" should {

    "return 1 for a leaf" in {
      val t: BinaryTree[Int] = Leaf(789)
      BinaryTree.size(t) shouldBe 1
      BinaryTree.sizeFoldingByTheBook(t) shouldBe 1
      BinaryTree.sizeFold(t) shouldBe 2
    }

    "return 3 for tree with 3 nodes" in {
      val t: BinaryTree[Int] = Branch(Leaf(1), Leaf(2))
      BinaryTree.size(t) shouldBe 3
      BinaryTree.sizeFoldingByTheBook(t) shouldBe 3
      BinaryTree.sizeFold(t) shouldBe 3
    }

    "return 7 when there's 7 bits" in {

      val t: BinaryTree[Int] = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(1), Leaf(2)))
      BinaryTree.sizeFoldingByTheBook(t) shouldBe 7
      BinaryTree.sizeFold(t) shouldBe 5
    }

    //    "return 3 for tree with 3 nodes" in {
    //      val t: Tree[Int] = Branch(Leaf(1), Leaf(2))
    //      Tree.size(t) shouldBe 3
    //      Tree.sizeFoldingByTheBook(t) shouldBe 3
    //    }
  }

  "Tree.maximum" should {

    "return the value of a leaf" in {

      val t = Leaf(7)
      BinaryTree.maximum(t) shouldBe 7
    }

    "return the highest value for a more complicated tree" in {

      val t = Branch(Leaf(8), Leaf(20))
      BinaryTree.maximum(t) shouldBe 20
    }
  }

  "Tree.depth" should {

    "return 0 for a single leaf" in {

      val t = Leaf(7)
      BinaryTree.depth(t) shouldBe 0
      BinaryTree.depth2(t) shouldBe 0
      BinaryTree.depthFoldingByTheBook(t) shouldBe 0
    }

    "return 2 for a tree with maximum depth 2" in {

      val t = Branch(Leaf(7), Branch(Leaf(8), Leaf(20)))
      BinaryTree.depth(t) shouldBe 2
      BinaryTree.depth2(t) shouldBe 2
      BinaryTree.depthFoldingByTheBook(t) shouldBe 2
    }
  }

  "Tree.map" should {

    "multiply a tree of ints by 2" in {

      val t = Branch(Leaf(7), Branch(Leaf(8), Leaf(20)))
      val expected = Branch(Leaf(14), Branch(Leaf(16), Leaf(40)))
      BinaryTree.map(t)(_ * 2) shouldBe expected
    }

    "return a tree of string lengths" in {

      val t = Branch(Leaf("One"), Branch(Leaf("Two"), Leaf("Three")))
      val expected = Branch(Leaf(3), Branch(Leaf(3), Leaf(5)))
      BinaryTree.map(t)(_.length) shouldBe expected
    }
  }

  "Tree.fold" should {

    "sum all the elements in a tree" in {

      val t = Branch(Leaf(7), Branch(Leaf(8), Leaf(20)))
      BinaryTree.fold[Int, Int](t, 0)(_ + _) shouldBe 35
    }

    "concatenate all the strings in a tree" in {

      val t = Branch(Leaf("One"), Branch(Leaf("Two"), Leaf("Three")))
      BinaryTree.fold[String, String](t, "")(_ + _) shouldBe "ThreeTwoOne"
    }
  }

  "Tree.foldAlt" should {

    "work exactly as Chris' version" in {
      val treeA: Branch[Int] = Branch(
        Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))),
        Branch(Leaf(5), Branch(Leaf(6), Leaf(7)))
      )

      BinaryTree.fold[Int, Int](treeA, 0)(_ + _) shouldBe 28
    }
  }

  "Tree.foldMap" should {

    "double all values in the tree" in {
      val treeA: Branch[Int] = Branch(
        Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))),
        Branch(Leaf(5), Branch(Leaf(6), Leaf(7)))
      )
      val expected: Branch[Int] = Branch(
        Branch(Leaf(2), Branch(Leaf(4), Branch(Leaf(6), Leaf(8)))),
        Branch(Leaf(10), Branch(Leaf(12), Leaf(14)))
      )

      BinaryTree.foldMap(treeA)(_ * 2) shouldBe expected
    }
  }

}
