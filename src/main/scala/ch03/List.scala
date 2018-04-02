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
    if(n < 1)
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
}
