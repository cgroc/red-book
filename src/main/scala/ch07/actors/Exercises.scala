package ch07.actors

import java.util.concurrent.{ExecutorService, Executors}

//import ch07.actors.Par.Par
import ch07.actors.Nonblocking.NPar

object Exercises {

  // TODO: kind of shitty because there's no guarantee that the number returned is in range
//  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
//    val choice: Int = Par.run(es)(n).get()
//    val parChoice: Par[A] = choices.drop(choice - 1).head
//    Par.run(es)(parChoice)
//  }

// If you do this then the whole damn list is calculated!
  def choiceN[A](n: NPar[Int])(choices: List[NPar[A]]): NPar[A] =
    NPar.flatMap[Int, A](n)(i => choices.drop(i - 1).head)

  def choice[A](cond: NPar[Boolean])(t: NPar[A], f: NPar[A]): NPar[A] = ???

  def displayLazyUnit[A](a: A): NPar[A] = {
    println(s"Calculating $a on ${Thread.currentThread().getName}")
    NPar.lazyUnit(a)
  }

  def main(args: Array[String]): Unit = {

    val es: ExecutorService = Executors.newFixedThreadPool(4)

    val iPar: NPar[Int] = choiceN(displayLazyUnit(3))(List(displayLazyUnit(1), displayLazyUnit(2), displayLazyUnit(3), displayLazyUnit(4)))

    val i: Int = NPar.run(es)(iPar)

    println(s"Voila, you have $i")
  }
}
