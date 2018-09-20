package ch07.sandbox

object EagerUnitPar {

  case class DisgustingCrap[A](a: A) {
    var maybeCrap: Option[A] = None

    def go(): Unit = {
      println(s"Computing $a")
      Thread.sleep(2000)
      maybeCrap = Some(a)
    }

    def get(): A = {
      println(s"Getting $a")
      while (maybeCrap == None)
        Thread.sleep(200)
      maybeCrap.get
    }

  }

  type Par[A] = DisgustingCrap[A]

  def unit[A](a: => A): Par[A] = {
    val crap = new DisgustingCrap[A](a)
    val t = new Thread(
      new Runnable {
        override def run(): Unit = {'\''
          crap.go()
        }
      }
    )
    t.start()
    crap
  }

  def get[A](a: Par[A]): A = a.get

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

object EagerApp {

  import ch07.sandbox.EagerUnitPar._

  def main(args: Array[String]): Unit = {
    timeSumAndPrint(IndexedSeq(1, 2, 3, 4))
  }
}