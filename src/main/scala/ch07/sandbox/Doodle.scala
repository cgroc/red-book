package ch07.sandbox

object Doodle {

  def sum(ints: IndexedSeq[Int]): Int =
    if(ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      sum(l) + sum(r)
    }

  // Dumb placeholders for now...
  // This is supposed to be some type that represents a parallel computation
  type Par[A] = Some[A]
  def unit[A](a: => A): Par[A] = Some(a)
  def get[A](a: Par[A]): A = a.get

  // Divide and conquer implementation of sum above can be done in parallel so let's see how
  // this might work
  def sum1(ints: IndexedSeq[Int]): Int =
    if(ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val parl = unit(sum(l))
      val parr = unit(sum(r))
      get(parl) + get(parr)
    }

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

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = ???

  def sum2(ints: IndexedSeq[Int]): Par[Int] =
    if(ints.length <= 1)
      unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val parl = unit(sum(l))
      val parr = unit(sum(r))
      map2(parl, parr)(_ + _)
    }

}
