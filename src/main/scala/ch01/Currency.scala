package ch01

trait Currency {
  def annotation: String

  def toString: String
}

object Currency {

  case object GBP extends Currency {
    override def annotation: String = "£"

    override def toString: String = annotation
  }

  case object USD extends Currency {
    override def annotation: String = "$"

    override def toString: String = annotation
  }

}