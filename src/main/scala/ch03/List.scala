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
    foldRight[A, List[A]](l1, l2)(Cons(_, _))

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

  // 3.16 Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be a
  // pure function that returns a new List!)
  def addOneToEverything(ints: List[Int]): List[Int] =
    ints match {
      case Nil => Nil
      case Cons(i, is) => Cons(i + 1, addOneToEverything(is))
    }

  // 3.17 Write a function that turns each value in a List[Double] into a String. You can use the expression d.toString
  // to convert some d: Double to a String.
  def makeAllTheDoublesInToStrings(doubles: List[Double]): List[String] =
    doubles match {
      case Nil => Nil
      case Cons(d, ds) => Cons(d.toString, makeAllTheDoublesInToStrings(ds))
    }

  //3.18
  // Write a function map that generalizes modifying each element in a list while maintain- ing the structure of the
  // list.

  def map[A, B](as: List[A])(f: A => B): List[B] =
    as match {
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), map(tail)(f))
    }


  // 3.19
  //  Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove
  // all odd numbers from a List[Int].
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Nil => Nil
      case Cons(head, tail) if (f(head)) => Cons(head, filter(tail)(f))
      case Cons(_, tail) => filter(tail)(f)
    }

  // 3.20
  //  Write a function flatMap that works like map except that the function given will return a list instead of a single
  //  result, and that list should be inserted into the final resulting list. Here is its signature:
  //  For instance, flatMap(List(1,2,3))(i => List(i,i)) should result in List(1,1,2,2,3,3).
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    val bs: List[List[B]] = map(as)(f)
    concatenateLists(bs)
  }

  // 3.21
  // Use flatMap to implement filter.
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

  // 3.22
  // Write a function that accepts two lists and constructs a new list by adding correspond- ing elements. For example,
  // List(1,2,3) and List(4,5,6) become List(5,7,9).
  def addCorrespondingEntries(first: List[Int], second: List[Int]): List[Int] = {
    @tailrec
    def loop(f: List[Int], s: List[Int], acc: List[Int]): List[Int] =
      (f, s) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(fhead, ftail), Cons(shead, stail)) => loop(ftail, stail, Cons(fhead + shead, acc))
      }
    reverse(loop(first, second, Nil)) //TODO: Do you really need to reverse? :-D
  }

  // 3.23
  // Generalize the function you just wrote so that it’s not specific to integers or addition. Name your generalized
  // function zipWith.
  def zipWith[A, B](first: List[A], second: List[A])(combine: (A, A) => B): List[B] = {
    @tailrec
    def loop(f: List[A], s: List[A], acc: List[B]): List[B] =
      (f, s) match {
        case (Nil, _) => acc
        case (_, Nil) => acc
        case (Cons(fhead, ftail), Cons(shead, stail)) => loop(ftail, stail, Cons(combine(fhead, shead), acc))
      }
    reverse(loop(first, second, Nil)) //TODO: Do you really need to reverse? :-D
  }

  // Thought this might help with hasSubsequence, was wrong but should still test/refine
  def takeWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) =>
        if (f(head))
          Cons(head, takeWhile(tail, f))
        else
          takeWhile(tail, f)
    }

  // 3.24
  // Hard: As an example, implement hasSubsequence for checking whether a List con- tains another List as a subsequence.
  // For instance, List(1,2,3,4) would have List(1,2), List(2,3), and List(4) as subsequences, among others. You may
  // have some difficulty finding a concise purely functional implementation that is also efficient. That’s okay.
  // Implement the function however comes most naturally. We’ll return to this implementation in chapter 5 and hopefully
  // improve on it. Note: Any two values x and y can be compared for equality in Scala using the expression x == y.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Nil, Cons(_,_)) => false
      case (Cons(_, supTail), Cons(subHead, _)) =>
        if(listLength(sub) > listLength(sup))
          false
        else {
          val matched: List[A] = dropWhile(sup, (x: A) => x != subHead)
          println(s"Matched: $matched")
          val matchesFound: Boolean = listLength(matched) > 0
          val zipped: List[Boolean] = zipWith(sub, matched)(_ == _)
          println(s"Zipped: $zipped")
          val allSubsequenceElementsMatched: Boolean = foldLeft(zipped, true)(_ && _)
          println(s"Evaluates as: $allSubsequenceElementsMatched")
          if(!matchesFound)
            false
          else if(allSubsequenceElementsMatched)
            true
          else
            hasSubsequence(supTail, sub)
        }
    }
  }


}
