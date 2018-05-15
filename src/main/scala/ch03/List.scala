package ch03

import scala.annotation.tailrec


sealed trait List[+A]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

case object Nil extends List[Nothing]

object List {

  def sum(ints: List[Int]): Int =
    ints match {
      case Nil => 0
      case Cons(head, tail) => head + sum(tail)
    }

  def product(doubles: List[Double]): Double =
    doubles match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(head, tail) => head * product(tail)
    }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // I suppose options for dealing with the Nil case are:
  // 1) Throw an exception (bleugh)
  // 2) Return Nil? This is what I decided to do here, could this be a problem?...
  // 3) Return an Option[List[A]] rather than List[A] - this is next chapter so I won't do it yet :-D
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(head, tail) => tail
    }

  def setHead[A](a: A, as: List[A]): List[A] =
    as match {
      case Nil => Nil //is this weird? Maybe it should be an exception?
      case Cons(head, tail) => Cons(a, tail)
    }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n < 1)
      l
    else
      drop(tail(l), n - 1)

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _ => l
    }

  def append[A](l1: List[A], l2: List[A]): List[A] =
    l1 match {
      case Nil => l2
      case Cons(head, tail) => Cons(head, append(tail, l2))
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }

  def foldRight[A, B](as: List[A], unit: B)(f: (A, B) => B): B =
    as match {
      case Nil => unit
      case Cons(head, tail) => f(head, foldRight(tail, unit)(f))
    }

  def sum2(ints: List[Int]): Int = foldRight[Int, Int](ints, 0)(_ + _)

  def product2(ints: List[Int]): Int = foldRight[Int, Int](ints, 1)(_ * _)

  def listLength[A](as: List[A]): Int = foldRight(as, 0)((_, b) => b + 1)

  def foldLeft[A, B](l: List[A], unit: B)(combine: (B, A) => B): B = {
    @tailrec
    def loop[A, B](as: List[A], z: B, acc: B)(f: (B, A) => B): B =
      as match {
        case Nil => acc
        case Cons(head, tail) => loop(tail, z, f(acc, head))(f)
      }

    loop(l, unit, unit)(combine)
  }

  def sumLeft(ints: List[Int]): Int = foldLeft[Int, Int](ints, 0)(_ + _)

  def productLeft(doubles: List[Double]): Double = foldLeft[Double, Double](doubles, 1)(_ * _)

  def listLengthLeft[A](as: List[A]): Int = foldLeft[A, Int](as, 0)((b, _) => b + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

  def foldRight2[A, B](as: List[A], unit: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), unit)((b, a) => f(a, b))
  }

  // implement foldLeft in terms of foldRight
  def foldLeft2[A, B](as: List[A], unit: B)(f: (B, A) => B): B =
    as match {
      case Nil => unit
      case Cons(head, tail) => f(foldRight(tail, unit)((a, b) => f(b, a)), head)
    }

  // 3.14
  // Implement append in terms of either foldLeft or foldRight.
  def appendLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft[A, List[A]](reverse(l1), l2)((b, a) => Cons(a, b))

  def appendRight[A](l1: List[A], l2: List[A]): List[A] =
    foldRight[A, List[A]](l1, l2)(Cons(_,_))

  // 3.15
  // Hard: Write a function that concatenates a list of lists into a single list.
  // Its runtime should be linear in the total length of all lists. Try to use functions we have already defined.
  // first attempt... Is this linear? I don't think so as appendLeft reverses the first list
  def concatenateLists[A](ls: List[List[A]]): List[A] = {
    def loop(lz: List[List[A]], acc: List[A]): List[A] =
      lz match {
        case Nil => acc
        case Cons(head, tail) => loop(tail, appendLeft(acc, head))
      }

    loop(ls, Nil)
  }

  // How about this? Is this linear?
  def concatenateLists2[A](ls: List[List[A]]): List[A] = {
    def loop(lz: List[List[A]], acc: List[A]): List[A] =
      lz match {
        case Nil => acc
        case Cons(head, tail) => loop(tail, appendRight(acc, head))
      }

    loop(ls, Nil)
  }



}
