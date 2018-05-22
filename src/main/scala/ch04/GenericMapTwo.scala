package ch04

import scala.util.Try

object GenericMapTwo {

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case _                    => None
    }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil    => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

  def parseInts(a: List[String]): Option[List[Int]] =
    sequence(a map (i => Try(i.toInt).toOption))

  // Exercise 4.5
  def traverse[A, B](as: List[A])(f: A => Maybe[B]): Maybe[List[B]] =
    as match {
      case Nil    => Just(Nil)
      case h :: t => f(h).flatMap(fh => traverse(t)(f).map(fh :: _))
    }

}
