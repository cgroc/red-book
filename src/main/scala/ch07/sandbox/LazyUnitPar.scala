package ch07.sandbox

object LazyUnitPar {

  case class TerriblePar[A](a: A) {
    var maybe: Option[A] = None

    def go(): Unit = {
      println(s"Computing $a")
      Thread.sleep(2000)
      maybe = Some(a)
    }

    def get(): A = {
      println(s"Getting $a")
      while (maybe == None)
        Thread.sleep(200)
      maybe.get
    }

  }

  type Par[A] = TerriblePar[A]

  def unit[A](a: => A): Par[A] = {
    new TerriblePar[A](a)
  }

  def get[A](a: Par[A]): A = {
    val t = new Thread(
      new Runnable {
        override def run(): Unit = {
          a.go()
        }
      }
    )
    t.start()
    a.get()
  }

  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val parl = unit(sum(l))
      val parr = unit(sum(r))
      get(parl) + get(parr)
    }

  def sum2(ints: IndexedSeq[Int]): Int =
    if (ints.length <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      get(unit(sum2(l))) + get(unit(sum2(r)))
    }

  def timeSumAndPrint(ints: IndexedSeq[Int]): Unit = {
    println("Running first function")
    val start1: Long = System.currentTimeMillis()
    val result1 = sum(ints)
    val stop1: Long = System.currentTimeMillis()
    val time = (stop1 - start1) / 1000
    println(s"First function took $time seconds and returned $result1")

    println("Running second function")
    val start2: Long = System.currentTimeMillis()
    val result2 = sum2(ints)
    val stop2: Long = System.currentTimeMillis()
    val time2 = (stop2 - start2) / 1000
    println(s"Second function took $time2 seconds and returned $result2")
  }
}

object LazyApp {

  import ch07.sandbox.LazyUnitPar._

  def main(args: Array[String]): Unit = {
    timeSumAndPrint(IndexedSeq(1, 2, 3, 4))
  }
}