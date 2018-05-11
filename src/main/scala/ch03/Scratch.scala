package ch03

import List._

object Scratch {
  def main(args: Array[String]): Unit = {

    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println(s"x is $x")

    //TODO: 3.7 can product via foldRight be short-circuited if any of the elements are 0.0?

    //TODO: 3.8 what does this say about the relationship between foldRight and the data constructors of List
    val wut = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

    println(wut)

    val wut2 = foldLeft(List(1,2,3), Nil:List[Int])((b, a) => Cons(a,b))

    println(wut2)

  }
}
