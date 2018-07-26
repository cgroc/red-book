package ch05.moi

//TODO: This came from an alvin alexander post I believe, find the link and post it here
object Test1WithFilterLazy extends App {

  def lessThan30(i: Int): Boolean = {
    println(s"\n$i less than 30?")
    i < 30
  }

  def moreThan20(i: Int): Boolean = {
    println(s"$i more than 20?")
    i > 20
  }

  val a = List(1, 25, 40, 5, 23)
  val q0 = a.withFilter(lessThan30)
  val q1 = q0.withFilter(moreThan20)
  for (r <- q1) println(s"$r")

}

/*
 * So my guess is that this will print:
 * 1 less than 20?
 * 1 more than 30?
 * 25 less than 20?
 * 25 more than 30?
 * 40 less than 20?
 * 40 more than 30?
 * 5 less than 20?
 * 5 more than 30?
 * 23 less than 20?
 * 23 more than 30?
 */
