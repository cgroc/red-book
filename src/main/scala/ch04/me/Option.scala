package ch04.me

sealed trait Option[+A] {

  // Exercise 4.1

  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }

  //TODO: apparently you don't need to pattern match for this
  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(a) => f(a)
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(value) => value
    }

  //TODO: or this
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(thing) => Some(thing)
    }

  //TODO: or this
  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(a) if f(a) => Some(a)
      case _ => None
    }

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {

  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f

  //Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _) => None
      case (_, None) => None
      case (Some(a), Some(b)) => Some(f(a,b))
    }
}