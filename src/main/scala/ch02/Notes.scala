package ch02

object Notes {
  // function literals in Scala are syntactic sugar! Functions
  // are objects, and so they're first class values

  val myGroovyFunction = new Function2[Int, Int, Int] {
    def apply(v1: Int, v2: Int): Int =
      (v1 + v2) * v1
  }

  def main(args: Array[String]): Unit = {
    println(myGroovyFunction(3, 2))
  }
}
