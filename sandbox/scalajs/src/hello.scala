package hello

import scala.scalajs.js

object HelloWorld {
  def main(args: Array[String]): Unit = {
    val p = js.Promise.resolve[Int](5)
    val r = js.async {
      3 * js.await(p)
    }
    js.async {
      println(js.await(r) + 1)
    }
  }
}
