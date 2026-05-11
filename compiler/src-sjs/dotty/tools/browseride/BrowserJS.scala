package dotty.tools.browseride

import scala.scalajs.js
import scala.scalajs.js.annotation.JSGlobal
import scala.scalajs.js.typedarray.Uint8Array

object BrowserJS:
  @js.native
  @JSGlobal("globalThis")
  private object GlobalThis extends js.Object

  @js.native
  @JSGlobal("TextDecoder")
  private class TextDecoder(label: String) extends js.Object:
    def decode(input: Uint8Array): String = js.native

  private lazy val utf8Decoder = new TextDecoder("utf-8")

  def global: js.Dynamic =
    GlobalThis.asInstanceOf[js.Dynamic]

  def decodeUTF8(bytes: Uint8Array): String =
    utf8Decoder.decode(bytes)

  def createModuleBlobURL(code: String): String =
    createBlobURL(code.asInstanceOf[js.Any], "text/javascript")

  def createBlobURL(part: js.Any, contentType: String): String =
    val options = js.Dynamic.literal()
    options.updateDynamic("type")(contentType)
    val blob = js.Dynamic.newInstance(global.selectDynamic("Blob"))(js.Array(part), options)
    global.selectDynamic("URL").createObjectURL(blob).toString

  def revokeObjectURL(url: String): Unit =
    global.selectDynamic("URL").revokeObjectURL(url)
