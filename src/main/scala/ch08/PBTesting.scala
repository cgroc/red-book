package ch08

object PBTesting {

  trait Gen[A]

  trait Prop {
    def check: Boolean

    // Exercise 8.3 - given a boolean representation of check, implement &&
    def &&(p: Prop): Prop =
      if(check) p else this
  }

  object Prop {

    def &&(p1: Prop, p2: Prop): Prop = new Prop {
      override def check: Boolean = p1.check && p2.check
    }
  }

  /*
   * I tried to do something like this and it didn't work, need to figure out why and how I get the references right
   *
   * def &&(p:Prop): Prop = new Prop {
   *  def check = this.check && p.check
   * }
   *
   * I think there's a scope issue with the 'this' keyword?
   */

  def listOf[A](ga: Gen[A]): Gen[List[A]] = ???

  def forAll[A](ga: Gen[A])(p: A => Boolean): Prop = ???

}

object PBTApp {
  import PBTesting._

  def main(args: Array[String]): Unit = {

    val propTrue: Prop = new Prop {
      override def check: Boolean = true
    }

    val propFalse: Prop = new Prop {
      override def check: Boolean = false
    }

    println(s"True and True: ${propTrue.&&(propTrue).check}")
    println(s"True and False: ${propTrue.&&(propFalse).check}")
    println(s"False and True: ${propFalse.&&(propTrue).check}")
    println(s"False and False: ${propFalse.&&(propFalse).check}")
  }
}