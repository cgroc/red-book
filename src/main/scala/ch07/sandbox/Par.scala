package ch07.sandbox

import java.util.concurrent.{ExecutorService, Future, TimeUnit}

object Par {

  val timeout = 5000L
  val timeUnits = TimeUnit.MILLISECONDS

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

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
      override def cancel(mayInterruptIfRunning: Boolean): Boolean = ???

      override def isCancelled: Boolean = ???

      override def isDone: Boolean = ???

      override def get(): C = {
        f(af.get(), bf.get())
      }

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
}

