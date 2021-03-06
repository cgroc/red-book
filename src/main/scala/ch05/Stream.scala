package ch05

import scala.annotation.tailrec

sealed trait Stream[+A] {

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(head, _) => Some(head())
    }

  // Exercise 5.1

  //Non stack safe version to warm up...
  //  def toList: List[A] =
  //    this match {
  //      case Empty => Nil
  //      case Cons(h, t) => h() :: t().toList
  //    }

  def toList: List[A] = {
    @tailrec
    def loop(s: Stream[A], acc: List[A]): List[A] =
      s match {
        case Empty => acc
        case Cons(h, t) => loop(t(), acc :+ h())
      }

    loop(this, Nil)
  }

  // Exercise 5.2 a
  def take(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case _ if n <= 0 => Empty
//      case Cons(h, _) if n == 1 => Cons(h, () => Empty)
      case Cons(h, t) => Cons(h, () => t().take(n - 1))
    }

  // Exercise 5.2 b
  def drop(n: Int): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) if n < 1 => Cons(h, t)
      case Cons(_, t) => t().drop(n - 1)
    }

  // Exercise 5.3
  def takeWhile(pred: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) if pred(h()) => Cons(h, () => t().takeWhile(pred))
      case Cons(_, _) => Empty
    }

  def exists(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

  def foldRight[B](unit: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(unit)(f))
      case Empty => unit
    }

  def existsViaFoldRight(p: A => Boolean): Boolean =
    this.foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhileViaFoldRight(pred: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A]) {
      (a, b) =>
        if (pred(a)) Stream.cons(a, b) else Stream.empty
    }

  // Exercise 5.6 (hard)
  def headOptionViaFoldRight: Option[A] =
    this.foldRight[Option[A]](None) {
      (a, _) => Some(a)
    }

  //Exercise 5.7, all in terms of foldRight
//  def map[B](f: A => B): Stream[B] =
//    this match {
//      case Empty => Empty
//      case Cons(h, t) => Cons(() => f(h()), () => t().map(f))
//    }


  def map[B](f: A => B): Stream[B] =
    this.foldRight[Stream[B]](Empty)((a, strb) => Stream.cons(f(a), strb))

  def filter(p: A => Boolean): Stream[A] =
    this.foldRight[Stream[A]](Empty) {
      (a, stra) =>
        if(p(a))
          Stream.cons(a, stra)
        else
          stra
    }

  def append[A1 >: A](s: => Stream[A1]): Stream[A1] =
    this.foldRight[Stream[A1]](s)((a, sa1) => Stream.cons(a, sa1))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight[Stream[B]](Empty)((a, strb) => f(a).append(strb))

  //From book
  def find(p: A => Boolean): Option[A] = filter(p).headOption

  // 5.13
  // Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll. The zipAll function should
  // continue the traversal as long as either stream has more elements—it uses Option to indicate whether each stream
  // has been exhausted.

  //def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = ???



//  def unfoldMap[B](f: A => B): Stream[B] =
//    Stream.unfold(this)(stream => stream.headOption map {
//      a => (f(a), stream.drop(1))
//    })
//
//  def unfoldTake(n: Int): Stream[A] =
//    Stream.unfold((n, this)) {
//      case (toTake, stream) => if(toTake == 0) None else stream.headOption.map(a => (a, (toTake - 1, stream.drop(1))))
//    }

  // def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] // reminder

  // I can take no credit for this
  def unfoldTakeWhile(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h.apply) => Some((h.apply, t.apply))
      case _ => None
    }


  def unfoldMap[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some(f(h.apply), t.apply)
      case _ => None
    }

  //  def zipWith[A, B](first: List[A], second: List[A])(combine: (A, A) => B): List[B]
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, s)) {
      case (Cons(ahead, atail), Cons(bhead, btail)) => Some((f(ahead.apply, bhead.apply), (atail.apply, btail.apply)))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah.apply), Some(bh.apply)), (at.apply, bt.apply))
      case (Cons(ah, at), Empty) => Some((Some(ah.apply), None), (at.apply, Empty))
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh.apply)), (Empty, bt.apply))
      case (Empty, Empty) => None
    }

  // hasSubsequence? TODO

  // 5.14
  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAll(s).takeWhile {
      case (_, Some(_)) => true
      case _ => false
    }.foldRight(true) {
      case ((maybeThis, maybeThat), acc) => {
        val stuffMatches: Boolean = (for {
          thisElem <- maybeThis
          thatElem <- maybeThat
        } yield thisElem == thatElem).getOrElse(false)
        stuffMatches && acc
      }
    }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // Exercise 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // Exercise 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // Exercise 5.10
  def fibs: Stream[Int] = {
    def next(nminus2: Int, nminus1: Int): Stream[Int] = {
      val current = nminus2 + nminus1
      cons(current, next(nminus1, current))
    }
    cons(0, cons(1, next(0, 1)))
  }

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons[A](a, unfold(s)(f))
    }

  // 5.12 write fibs, from, constant and ones in terms of unfold
  // Just to make it totally clear to myself... break it in to bits?
  def unfoldFibs: Stream[Int] = {
    val generate: (Int, Int) => Option[(Int, (Int, Int))] =
      (first, second) => Some((first, (second, first + second)))
//    unfold[Int, (Int, Int)]((0, 1))(generate) // why the heck doesn't this compile?
    unfold[Int, (Int, Int)]((0, 1)) {
      t => Some(t._1, (t._2, t._1 + t._2))
    }
  }

  def unfoldFrom(n: Int): Stream[Int] =
    unfold[Int, Int](n)(m => Some(m, m + 1))

  def unfoldConstant[A](value: A): Stream[A] = unfold(value)(c => Some((c, c)))

  def unfoldOnes: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  // Just for funsies
  def unfoldFactorial: Stream[Int] =
    unfold(1, 1) {
      t => Some(t._1, (t._1 * t._2, t._2 + 1))
    }

  def unfoldFactorialLessHorrible: Stream[Int] =
    unfold(1, 1) {
      case (acc, current) =>  Some(acc, (acc * current, current + 1)) // you can give somewhat more meaningful names to the tuple things
    }

}

object Main extends App {

  println("Stream(1, 2, 3, 4, 5) is: " + Stream(1, 2, 3, 4, 5))
  println("Stream(1, 2, 3, 4, 5).toList is: " + Stream(1, 2, 3, 4, 5).toList)
  println()

  println("Stream(5, 4, 3, 2, 1).take(3) is: " + Stream(5, 4, 3, 2, 1).take(3))
  println("Stream(5, 4, 3, 2, 1).take(3).toList is: " + Stream(5, 4, 3, 2, 1).take(3).toList)
  println()

  println("Stream(5, 4, 3, 2, 1).drop(2) is: " + Stream(5, 4, 3, 2, 1).drop(2))
  println("Stream(5, 4, 3, 2, 1).drop(2).toList is: " + Stream(5, 4, 3, 2, 1).drop(2).toList)
  println()

  println("Stream(5, 4, 3, 2, 1).takeWhile(_ > 3) is: " + Stream(5, 4, 3, 2, 1).takeWhile(_ > 3))
  println("Stream(5, 4, 3, 2, 1).takeWhile(_ > 3).toList is: " + Stream(5, 4, 3, 2, 1).takeWhile(_ > 3).toList)
  println()

  println("Stream(1, 2, 3, 4, 5).forAll(_ < 2) is: " + Stream(1, 2, 3, 4, 5).forAll(a => {
    println(a); a < 2
  }))
  println()

  println("Stream(1, 2, 3, 4, 5).forAll(_ > 0) is: " + Stream(1, 2, 3, 4, 5).forAll(a => {
    println(a); a > 0
  }))
  println()

  println("Stream(5, 4, 3, 2, 1).takeWhileViaFoldRight(_ > 3) is: " + Stream(5, 4, 3, 2, 1).takeWhileViaFoldRight(_ > 3))
  println("Stream(5, 4, 3, 2, 1).takeWhileViaFoldRight(_ > 3).toList is: " + Stream(5, 4, 3, 2, 1).takeWhileViaFoldRight(_ > 3).toList)
  println()

  println("Stream(5, 4, 3, 2, 1).headOption is: " + Stream(5, 4, 3, 2, 1).headOption)
  println("Stream(5, 4, 3, 2, 1).headOptionViaFoldRight is: " + Stream(5, 4, 3, 2, 1).headOptionViaFoldRight)
  println()

  println("Stream.empty.headOption is: " + Stream.empty.headOption)
  println("Stream.empty.headOptionViaFoldRight is: " + Stream.empty.headOptionViaFoldRight)
  println()


  println("Stream(5, 4, 3, 2, 1).map(_ - 1) is: " + Stream(5, 4, 3, 2, 1).map(_ - 1).toList)
  println()

  println("Stream(5, 4, 3, 2, 1).filter(_ < 3) is: " + Stream(5, 4, 3, 2, 1).filter(_ < 3).toList)
  println()

  println("Stream(5, 4, 3).append(Stream(2, 1)) is: " + Stream(5, 4, 3).append(Stream(2, 1)).toList)
  println()

  println("Stream(5, 4, 3, 2, 1).flatMap(i => Stream(i * 2)) is: " + Stream(5, 4, 3, 2, 1).flatMap(i => Stream(i * 2)).toList)
  println()

  val ones: Stream[Int] = Stream.cons(1, ones)

  println("Five 1s: " + ones.take(5).toList)
  println()

  println("Using constant: " + Stream.constant("Hello").take(5).toList)
  println()

  println("From 7 take 8: " + Stream.from(7).take(8).toList)
  println()

  println("From 7 take 8 (using unfold): " + Stream.unfoldFrom(7).take(8).toList)
  println()

  println("First 7 fibs: " + Stream.fibs.take(7).toList)
  println()

  println("First 7 fibs (using unfold): " + Stream.unfoldFibs.take(7).toList)
  println()

  println("unfoldConstant: " + Stream.unfoldConstant("MY BRAIN HURTS").take(3).toList)
  println()

  println("unfoldOnes: " + Stream.unfoldOnes.take(6).toList)
  println()

  println("factorial: " + Stream.unfoldFactorial.take(10).toList)
  println()

//  println("factorial (use unfoldTake): " + Stream.unfoldFactorial.unfoldTake(10).toList)
  println()

  println("factorial: " + Stream.unfoldFactorialLessHorrible.take(10).toList)
  println()

//  println("Stream(5, 4, 3, 2, 1).unfoldMap(_ - 1) is: " + Stream(5, 4, 3, 2, 1).unfoldMap(_ - 1).toList)
  println()

  println("From 1 take while < 10 (using unfold): " + Stream.unfoldFrom(1).unfoldTakeWhile(_ < 10).toList)
  println()

  println("From an empty stream take while < 10 (using unfold): " + Stream.empty[Int].unfoldTakeWhile(_ < 10).toList)
  println()

}