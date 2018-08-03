package ch06

object DumbExample {

  case class Counter(count: Int)

//  type CounterState[Int] = State[Counter, Int]

  def doSummat(current: Int, until: Int, state: State[Counter, String], counter: Counter): Unit = {
    if(current >= until)
      ()
    else {
      val (s, c) = state.run(counter)
      println(s)
      doSummat(current + 1, until, state, c)
    }
  }

  def main(args: Array[String]): Unit = {
    val myDumbCounter = State[Counter, Int](c => (c.count, c.copy(count = c.count + 1)))
    val nagger: State[Counter, String] = myDumbCounter.map[String](i =>"Nag" * i)


//    val (output, state) = nagger.run(Counter(5))
//
//    println("out is " + output + " state is now" + state)
//
//
    doSummat(0, 5, nagger, Counter(1))

  }
}