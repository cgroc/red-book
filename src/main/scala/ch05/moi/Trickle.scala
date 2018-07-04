package ch05.moi

sealed trait Trickle[A]

final case class TricklePair[A](head: () => A, tail: Trickle[A]) extends Trickle[A]

final case class TrickleNowt[A]() extends Trickle[A]



object main extends App {

  def doItTwice(it: => Int): Int = it + it

  def dontDoItTwice(it: => Int): Int = {
    lazy val theThing: Int = it
    theThing + theThing
  }

  def it: Int = {
    println("I'm doing a thing there")
    7
  }

  println("hello")

  println("\n===== Doing it twice =====\n")

  val firstResult = doItTwice(it)

  println(s"\nThat evaluated to $firstResult")

  println("\n===== Not doing it twice =====\n")

  val secondResult = dontDoItTwice(it)

  println(s"\nThat evaluated to $secondResult")


}