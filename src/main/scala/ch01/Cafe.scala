package ch01

class Cafe {

  def buyCoffees(creditCard: CreditCard,
                 myCoffee: List[Coffee]): (List[Coffee], Charge) = {
    val (coffees, charges) = myCoffee.map(buyCoffee(creditCard)).unzip
    (coffees, charges.reduceOption(_ + _).getOrElse(Charge(creditCard, 0)))
  }

  def buyCoffee(creditCard: CreditCard)(coffee: Coffee): (Coffee, Charge) =
    (coffee, Charge(creditCard, coffee.price))

}

case class Charge(cc: CreditCard, amount: Double) {

  def +(other: Charge): Charge =
    if (cc == other.cc) Charge(cc, amount + other.amount)
    else throw new Exception("Can't combine charges to different cards.")

  override def toString: String = f"$amount%1.2f"

  def coalesce(charges: List[Charge]): List[Charge] =
    charges.groupBy(_.cc).values.flatMap(_.reduceOption(_ + _).toList).toList

}

class CreditCard {
  def charge(amount: Double): Unit = println(s"Charging card $amount")
}

//object Kiosk extends App {
//
//  val currencyOne: Currency = GBP
//
//  val dansCafe: Cafe = new Cafe()
//
//  val billsMasterCard: CreditCard = new CreditCard()
//
//  val billsCoffee: List[Coffee] = List[Coffee](Expresso, LongBlack, FlatWhite, FlatWhite)
//
//  val (coffeesPurchased: List[Coffee], receipt: Charge) = dansCafe.buyCoffees(billsMasterCard, billsCoffee)
//  println(s"Coffees paid for: $coffeesPurchased for $currencyOne$receipt")
//
//}
