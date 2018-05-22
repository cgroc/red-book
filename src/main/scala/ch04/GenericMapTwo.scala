package ch04

object GenericMapTwo {

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case _                    => None
    }

}
