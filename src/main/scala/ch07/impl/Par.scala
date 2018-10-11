package ch07.impl

trait Par[A] {

  def unit(a: A): Par[A]

  def map2[B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C]

  def fork(a: => Par[A]): Par[A]

  def lazyUnit(a: => A): Par[A] = fork(unit(a))

  def run(a: Par[A]): A
}
