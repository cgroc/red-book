package ch03

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // 3.25
  // Write a function size that counts the number of nodes (leaves and branches) in a tree.
  def size[A](tree: Tree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
    }

  // 3.26
  // Write a function maximum that returns the maximum element in a Tree[Int]. (Note: In Scala, you can use x.max(y) or
  // x max y to compute the maximum of two integers x andy.)
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(i) => i
      case Branch(l, r) =>
        val (maxl, maxr): (Int, Int) = (maximum(l), maximum(r))
        if(maxl > maxr) maxl else maxr
    }

  // 3.27
  // Write a function depth that returns the maximum path length from the root of a tree to any leaf.
  def depth[A](tree: Tree[A]): Int = {
    def loop(t: Tree[A], acc: Int): Tree[Int] =
      t match {
        case Leaf(_) => Leaf(acc)
        case Branch(l, r) => Branch(loop(l, acc + 1), loop(r, acc + 1))
      }
    maximum(loop(tree, 0))
  }

  // 3.28
  // Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with
  // a given function.
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Leaf(value) => Leaf(f(value))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // 3.29
  // Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
  // Reimplement them in terms of this more general function. Can you draw an analogy between this fold function and the
  // left and right folds for List?
  def fold[A, B](tree: Tree[A], unit: B)(combine: (A, B) => B): B = {
    def loop(t: Tree[A], acc: B): B =
      t match {
        case Leaf(a) => combine(a, acc)
        case Branch(l, r) =>
          val newacc: B = loop(l, acc)
          loop(r, newacc)
      }
    loop(tree, unit)
  }
}