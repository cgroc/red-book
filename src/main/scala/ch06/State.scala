package ch06

final case class State[S, +A](run: S => (A, S)) {

  import State._
  //
  //  def unit(a: A): State[S, A] =
  //    State(s => (a, s))

  def map[B](f: A => B): State[S, B] =
    State(
      s => {
        val (a1, s1) = run(s)
        (f(a1), s1)
      }
    )

  def map2[B, C](otherState: State[S, B])(f: (A, B) => C): State[S, C] = ???

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(
      s => {
        val (a1, s2): (A, S) = run(s)
        f(a1).run(s2)
      }
    )


}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    l.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

}


