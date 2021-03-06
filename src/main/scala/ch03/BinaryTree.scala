package ch03

sealed trait BinaryTree[+A]

case class Leaf[A](value: A) extends BinaryTree[A]

case class Branch[A](left: BinaryTree[A], right: BinaryTree[A])
    extends BinaryTree[A]

object BinaryTree {

  // 3.25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size[A](tree: BinaryTree[A]): Int =
    tree match {
      case Leaf(_)             => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  // 3.26
  // Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you can use x.max(y) or
  // x max y to compute the maximum of two integers x andy.)
  def maximum(t: BinaryTree[Int]): Int =
    t match {
      case Leaf(i) => i
      case Branch(l, r) =>
        val (maxl, maxr): (Int, Int) = (maximum(l), maximum(r))
        if (maxl > maxr) maxl else maxr
    }

  // 3.27
  // Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  def depth[A](tree: BinaryTree[A]): Int = {
    def loop(t: BinaryTree[A], acc: Int): BinaryTree[Int] =
      t match {
        case Leaf(_)      => Leaf(acc)
        case Branch(l, r) => Branch(loop(l, acc + 1), loop(r, acc + 1))
      }
    maximum(loop(tree, 0))
  }

  def depth2[A](tree: BinaryTree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) =>
        val (depthl, depthr): (Int, Int) = (depth2(l) + 1, depth2(r) + 1)
        depthl max depthr
    }

  // 3.28
  // Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with
  // a given function.
  def map[A, B](tree: BinaryTree[A])(f: A => B): BinaryTree[B] =
    tree match {
      case Leaf(value)  => Leaf(f(value))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // 3.29
  // Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  // Reimplement them in terms of this more general function. Can you draw an analogy between this fold function and the
  // left and right folds for List?
  def fold[A, B](tree: BinaryTree[A], unit: B)(combine: (A, B) => B): B = {
    def loop(t: BinaryTree[A], acc: B): B =
      t match {
        case Leaf(a) => combine(a, acc)
        case Branch(l, r) =>
          val newacc: B = loop(l, acc)
          loop(r, newacc)
      }
    loop(tree, unit)
  }

  def sizeFold[A](tree: BinaryTree[A]): Int =
    fold(tree, 1)((_, int) => int + 1)

  // I'm not sure there's any way to do depth like this!
//  def depthFold[A](tree: Tree[A]): Int =
//    fold(tree, 0)((_, int) => int + 1)

  def foldByTheBook[A, B](tree: BinaryTree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Leaf(a)      => f(a)
      case Branch(l, r) => g(foldByTheBook(l)(f)(g), foldByTheBook(r)(f)(g))
    }

  def sizeFoldingByTheBook[A](tree: BinaryTree[A]): Int =
    foldByTheBook[A, Int](tree)(_ => 1)(_ + _ + 1)

  def depthFoldingByTheBook[A](tree: BinaryTree[A]): Int =
    foldByTheBook[A, Int](tree)(_ => 0)((ldepth: Int, rdepth: Int) =>
      (ldepth + 1) max (rdepth + 1))

  // Alternative solutions for Fold and FoldMap
  def foldAlt[A, B](as: BinaryTree[A])(f: A => B)(g: (B, B) => B): B =
    as match {
      case Leaf(v)      => f(v)
      case Branch(l, r) => g(foldAlt(l)(f)(g), foldAlt(r)(f)(g))
    }

  def foldMap[A, B](as: BinaryTree[A])(f: A => B): BinaryTree[B] =
    foldAlt[A, BinaryTree[B]](as)(a => Leaf(f(a)))(Branch(_, _))

}
