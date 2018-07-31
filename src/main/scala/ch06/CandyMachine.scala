package ch06

sealed trait CandyMachine {

  def insertCoin: (CoinInsertionResult, CandyMachine)
  def turnHandle: (CandyDispensingResult, CandyMachine)
}

// TODO: Non-negative integers as types for fields?
// TODO: Maybe a bit silly, but candyRemaining + coins should be an invariant. Is it possible to track this?
case class UnlockedCandyMachine(candyRemaining: Int, coins: Int) extends CandyMachine {

  def insertCoin: (CoinInsertionResult, CandyMachine) =
    (CoinInsertionResult.Failure("Coin already inserted"), this)

  def turnHandle: (CandyDispensingResult, CandyMachine) =
    (
      CandyDispensingResult.Success,
      if(candyRemaining == 1)
        EmptyCandyMachine(coins)
      else
        LockedCandyMachine(candyRemaining - 1, coins)
    )
}

case class LockedCandyMachine(candyRemaining: Int, coins: Int) extends CandyMachine {

  def insertCoin: (CoinInsertionResult, CandyMachine) =
    (CoinInsertionResult.Success, UnlockedCandyMachine(candyRemaining, coins + 1))

  def turnHandle: (CandyDispensingResult, CandyMachine) =
    (CandyDispensingResult.Failure("No coin inserted"), this)
}

case class EmptyCandyMachine(coins: Int) extends CandyMachine {

  def insertCoin: (CoinInsertionResult, CandyMachine) =
    (CoinInsertionResult.Failure("Machine is empty"), this)

  def turnHandle: (CandyDispensingResult, CandyMachine) =
    (CandyDispensingResult.Failure("Machine is empty"), this)
}

//TODO: Is there any need for this?
sealed trait CoinInsertionResult

object CoinInsertionResult {

  case object Success extends CoinInsertionResult
  case class Failure(reason: String) extends CoinInsertionResult //TODO: Something better than a string?
}

//TODO: Would an option do just as well?
sealed trait CandyDispensingResult

object CandyDispensingResult{

  case object Success extends CandyDispensingResult
  case class Failure(reason: String) extends CandyDispensingResult //TODO: Something better than a string?
}
