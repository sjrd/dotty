package dotty.tools.sjs

import scala.scalajs.js

object JSInterop:
  def isDefined(value: js.Any): Boolean =
    value != null && !js.isUndefined(value)

  def stringOr(value: js.Any, fallback: String): String =
    if isDefined(value) then value.toString else fallback

  def intValue(value: js.Any): Int =
    js.typeOf(value) match
      case "number" => value.asInstanceOf[Double].toInt
      case "string" => value.asInstanceOf[String].toIntOption.getOrElse(0)
      case _        => 0

  def stringArray(value: js.Any): Seq[String] =
    if js.Array.isArray(value) then
      value.asInstanceOf[js.Array[js.Any]].toSeq.map(value => stringOr(value, "").trim).filter(_.nonEmpty)
    else Nil

  def dynamicArray(value: js.Any): Seq[js.Dynamic] =
    if js.Array.isArray(value) then value.asInstanceOf[js.Array[js.Dynamic]].toSeq else Nil
