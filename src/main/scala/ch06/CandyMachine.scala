package ch06

sealed trait CandyMachine {

  val processInput: Input => CandyMachine = { // ugh
    case Coin => insertCoin
    case Turn => turnHandle
  }

  def insertCoin: CandyMachine
  def turnHandle: CandyMachine
}

// TODO: Non-negative integers as types for fields?
// TODO: Maybe a bit silly, but candyRemaining + coins should be an invariant. Is it possible to track this?
case class UnlockedCandyMachine(candyRemaining: Int, coins: Int) extends CandyMachine {

  def insertCoin: CandyMachine =
    this

  def turnHandle: CandyMachine =
    if (candyRemaining == 1)
      EmptyCandyMachine(coins)
    else
      LockedCandyMachine(candyRemaining - 1, coins)
}

case class LockedCandyMachine(candyRemaining: Int, coins: Int) extends CandyMachine {

  def insertCoin: CandyMachine =
    UnlockedCandyMachine(candyRemaining, coins + 1)

  def turnHandle: CandyMachine =
    this
}

case class EmptyCandyMachine(coins: Int) extends CandyMachine {

  def insertCoin: CandyMachine =
    this

  def turnHandle: CandyMachine =
    this
}

sealed trait Input

case object Coin extends Input
case object Turn extends Input

//TODO: Is there any need for this?
//sealed trait CoinInsertionResult
//
//object CoinInsertionResult {
//
//  case object Success extends CoinInsertionResult
//  case class Failure(reason: String) extends CoinInsertionResult //TODO: Something better than a string?
//}
//
////TODO: Would an option do just as well?
//sealed trait CandyDispensingResult
//
//object CandyDispensingResult{
//
//  case object Success extends CandyDispensingResult
//  case class Failure(reason: String) extends CandyDispensingResult //TODO: Something better than a string?
//}

object Foo {

//  def simulateMachine(candyMachine: CandyMachine, inputs: List[Input]): CandyMachine = {
//    inputs.foldLeft[CandyMachine](candyMachine) {
//      (c, i) => c.processInput(i)
//    }
//  }

  // def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]]
  // CandyMachine => ((Int, Int), CandyMachine)
  def simulateMachine(inputs: List[Input]): State[CandyMachine, (Int, Int)] = {
    // input -> List[CandyMachine, Input]
    // inputs
    ???
  }
}

object Bar extends App {

//  println(Foo.simulateMachine(LockedCandyMachine(10, 0), List(Coin, Turn, Coin, Turn, Coin, Turn)))
//  println(Foo.simulateMachine(EmptyCandyMachine(5), List(Coin, Turn, Coin, Turn, Coin, Turn)))
}
