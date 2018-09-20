package ch07.sandbox

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Doodle {

  import scala.concurrent.ExecutionContext.Implicits.global

  def sum(ints: IndexedSeq[Int]): Int =
    if(ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  // Dumb placeholders for now...
  // This is supposed to be some type that represents a parallel computation
  //type Par[A] = Some[A]
  //def unit[A](a: => A): Par[A] = Some(a)
  //def get[A](a: Par[A]): A = a.get

  def unit[A](a: => A): Par[A] = Par.Unit(() => a)

  sealed trait Par[A] {
    def getSeq: A = this match {
      case Par.Unit(f) => f()
      case Par.Map2(a, b, f) => f(a.getSeq, b.getSeq)
    }

    def toFuture: Future[A] = this match {
      case Par.Unit(f) => Future { f() }
      case Par.Map2(a, b, f) => {
        val fa = a.toFuture
        val fb = b.toFuture

        for {
          a <- fa
          b <- fb
        } yield {
          f(a, b)
        }
      }
    }

    def get: A = Await.result(toFuture, Duration.Inf)
  }

  object Par {
    case class Unit[A](v: () => A) extends Par[A]
    case class Map2[A](parLeft: Par[A], parRight: Par[A], map: (A, A) => A) extends Par[A]
  }

  // Divide and conquer implementation of sum above can be done in parallel so let's see how
  // this might work
  /*
  def sum1(ints: IndexedSeq[Int]): Int =
    if(ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val parl = unit(sum(l))
      val parr = unit(sum(r))
      get(parl) + get(parr)
    }
    */

  // TODO: review the ideas here. I'm hazy!
  // However, this gets tricky! Should unit eagerly evaluate its argument or wait until get is called before executing?
  // It looks like we want unit to eagerly evaluate above, reason being that if we wait until get is called then we get
  // sequential execution! (Strict left to right evaluation means that get has to wait until the Par is completed before
  // moving on to the right hand side of the sum.
  //
  // HOWEVER! even if you eagerly evaluate you break referential transparency here. Substitution of the parl and parr
  // values in to the last line of sum1 gives this:
  //
  // get(unit(sum(l))) + get(unit(sum(l)))
  //
  // which executes sequentially for the same reason!

  // What's the way out of this? We have a side effect with respect to get and unit which we want to push to the very
  // edge of our programme. We can achieve this if our sum function incrementally builds up a Par[A] value - i.e. we
  // return a Par from our function. This means we need a new combinator function, map2!

  def map2[A](a: Par[A], b: Par[A])(f: (A, A) => A): Par[A] = Par.Map2(a, b, f)

  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    if(ints.length <= 1)
      unit(ints.headOption getOrElse 0)
    else {
//      ints.aggregate(0)(_ + _,_ + _)
      val (l, r) = ints.splitAt(ints.length / 2)
      val parl = unit(sum(l))
      val parr = unit(sum(r))
      map2(parl, parr)(_ + _)
    }

}

object Diogo extends App {

  import Doodle._

  val summedNumbers = sum2(IndexedSeq(1, 2, 3, 4)).get

  println(summedNumbers)
}
