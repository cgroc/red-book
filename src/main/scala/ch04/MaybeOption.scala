package ch04

import ch04.Maybe.Absent

sealed trait Maybe[+A] {

  def map[B](f: A => B): Maybe[B] = this match {
    case Absent  => Absent
    case Just(a) => Just(f(a))
  }

  def flatMap[B](f: A => Maybe[B]): Maybe[B] = this match {
    case Absent  => Absent
    case Just(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Just(a) => a
    case _       => default
  }

  def orElse[B >: A](ob: => Maybe[B]): Maybe[B] = this match {
    case Absent => ob
    case _      => this
  }

  def filter(f: A => Boolean): Maybe[A] = this match {
    case Just(a) if f(a) => this
    case _               => Absent
  }

  def isDefined: Boolean = this match {
    case Absent => false
    case _      => true
  }

  def get: A

}

case class Just[A](get: A) extends Maybe[A]

object Maybe {

  def apply[A](a: A): Maybe[A] = Just[A](a)

  case object Absent extends Maybe[Nothing] {
    def get = throw new UnsupportedOperationException("Method undefined")
  }

}
