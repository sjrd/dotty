package hello

import scala.annotation.static

class Invoker

object Invoker:
  @static val x: String = "hello"
  @static def f(): String = x

object HelloWorld {
  def main(args: Array[String]): Unit = {
    val x = Invoker.x
    println(x)
    val y = Invoker.f()
    println(y)
  }
}
