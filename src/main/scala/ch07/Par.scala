package ch07

import java.util
import java.util.concurrent._

import language.implicitConversions

//object Par {
//
//  trait Par[A]
//
//  def unit[A](a: => A): Par[A] = ???
//
//  def get[A](a: Par[A]): A = ???
//
//  //Exercise 7.1
//
//  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???
//}

object Par {

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    //val af = a(es)
    //val bf = b(es)
//    UnitFuture(f((fork(a)(es), fork(b)(es))))
    ???
  }

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })
}
