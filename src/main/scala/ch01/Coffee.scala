package ch01

trait Coffee {
  def price: Double
}

object Coffee {

  case object Expresso extends Coffee {
    override val price: Double = 1.20
  }

  case object LongBlack extends Coffee {
    override val price: Double = 1.20
  }

  case object FlatWhite extends Coffee {
    override val price: Double = 1.20
  }

}
