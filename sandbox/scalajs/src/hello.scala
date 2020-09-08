package hello

import scala.scalajs.js
import scala.scalajs.js.annotation._

class Foo(val x: Int) extends js.Object {
  def bar(y: Int): Int = x + y
}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println("hello dotty.js!")
    val obj = new Foo(5)
    println(obj.x)
    println(obj.bar(6))
  }
}
