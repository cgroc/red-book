package ch07.actors

import java.util.concurrent.{ExecutorService, Executors}

//import ch07.actors.Blocking.Par
import ch07.actors.Nonblocking.Par

object Exercises {

  // TODO: kind of shitty because there's no guarantee that the number returned is in range
//  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
//    val choice: Int = Par.run(es)(n).get()
//    val parChoice: Par[A] = choices.drop(choice - 1).head
//    Par.run(es)(parChoice)
//  }

//  7.11
// Why is this executing on main thread and why does everything get calculated?
  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    Par.flatMap[Int, A](n)(i => choices(i - 1))
//    Par.flatMap[Int, A](n)(i => choices.drop(i - 1).head)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(Par.map(cond)(if(_) 1 else 2))(List(t, f))

  def choiceMap[K,V](key: Par[K])(choices: Map[K,Par[V]]): Par[V] = ???

  def choiceFMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] = ???

  def displayLazyUnit[A](a: A): Par[A] = Par.lazyUnit {
    println(s"Calculating $a on ${Thread.currentThread().getName}")
    a
  }

  def main(args: Array[String]): Unit = {

    val es: ExecutorService = Executors.newFixedThreadPool(4)

    val iPar: Par[Int] = choiceN(displayLazyUnit(3))(List(displayLazyUnit(11), displayLazyUnit(12), displayLazyUnit(13), displayLazyUnit(14)))

    val i: Int = Par.run(es)(iPar)

    println(s"Voila, you have $i")

    val sPar: Par[String] = choice(displayLazyUnit(true))(displayLazyUnit("True!"), displayLazyUnit("False!"))

    val s = Par.run(es)(sPar)

    println(s)

    es.shutdown()

  }
}
