package externalmacros

import scala.quoted.*

object ExternalMacros:
  inline def label(inline value: String): String = ${ labelImpl('value) }

  def labelImpl(value: Expr[String])(using Quotes): Expr[String] =
    Expr(s"external macro: ${value.valueOrAbort.toUpperCase}")
