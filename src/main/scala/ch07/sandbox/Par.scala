package ch07.sandbox

import java.util.concurrent._

import scala.collection.immutable

object Par {

  val timeout = 5000L
  val timeUnits = TimeUnit.MILLISECONDS

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = es => es.submit(new Callable[A] {
    def call = a(es).get
  })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit) = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
    val af: Future[A] = a(es)
    val bf: Future[B] = b(es)

    new Future[C]() {
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = true

      override def isCancelled: Boolean = af.isCancelled || bf.isCancelled

      override def isDone: Boolean = af.isDone && bf.isDone

      override def get(): C = {
        f(af.get(), bf.get())
      }

      //Respect contract on timeouts
      override def get(timeout: Long, unit: TimeUnit): C = {
        val start = System.currentTimeMillis()
        val aRes = af.get(timeout, unit)
        val end = System.currentTimeMillis()

        val asMilliseconds = unit.toMillis(timeout)
        val timeTakenForA = end - start

        val bRes = bf.get(asMilliseconds - timeTakenForA, unit)
        f(aRes, bRes)
      }
    }

  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def map[A, B](a: Par[A])(f: A => B): Par[B] =
    map2(a, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List.empty))((a1, a2) => map2(a1, a2)(_ :: _))

  def sequence2[A](ps: List[Par[A]]): Par[List[A]] = es => {
    val res: immutable.Seq[A] = ps.map(par => par(es)).map(_.get)
    UnitFuture(res.toList)
  }

  def sequence3[A](ps: List[Par[A]]): Par[List[A]] =
    ps match {
      case Nil => Par.unit(List.empty)
      case h :: Nil => map(h)(List.apply(_))
      case _ =>
        val (l, r) = ps.splitAt(ps.length / 2)
        map2(sequence3(l), sequence3(r))(_ ++ _)
    }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val wot: List[Par[List[A]]] = as.map(
      asyncF(
        a =>
          if (f(a))
            List(a)
          else
            List.empty
      ))
    val ugh: Par[List[List[A]]] = sequence(wot)
    map(ugh)(_.flatten)
  }

}

object ParApp extends App {

  import Par._

  val es = Executors.newFixedThreadPool(8)

  val intList: List[Int] = List(5, 1, 3, 7, 2, 8, 9, 4, 6)

  val intListPar: Par[List[Int]] = unit(intList)

  val sortedIntListPar: Par[List[Int]] = map(intListPar)(_.sorted)

  println(sortedIntListPar)

  //  println(run(es)(sortedIntListPar).get())

  def sum(lst: List[Int]): Par[Int] =
    if (lst.length <= 1) {
      println("Running on: " + Thread.currentThread().getName)
      lazyUnit(lst.headOption.getOrElse(0))
    }
    else {
      val (l, r) = lst.splitAt(lst.length / 2)
      map2(sum(l), sum(r))(_ + _)
    }

  val sumPar: Par[Int] = sum(intList)

  println(run(es)(sumPar).get())

  val listPar = List(unit(1), unit(2), unit(3))

  val parList: Par[List[Int]] = sequence(listPar)

  println(listPar)
  println(parList)

  println(run(es)(parList))

  val list = sequence3(List(unit(0), unit(1), unit(2), unit(3)))(es)

  println(list.get(0, TimeUnit.NANOSECONDS))

  es.shutdown()

}

