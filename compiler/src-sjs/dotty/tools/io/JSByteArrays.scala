package dotty.tools.io

import dotty.tools.sjs.JSInterop

import scala.scalajs.js
import scala.scalajs.js.typedarray.{ArrayBuffer, Int8Array, Uint8Array}

object JSByteArrays:
  def toUint8Array(bytes: Array[Byte]): Uint8Array =
    val arr = new Uint8Array(bytes.length)
    var i = 0
    while i < bytes.length do
      arr(i) = (bytes(i) & 0xff).toShort
      i += 1
    arr

  def toByteArray(bytes: Uint8Array): Array[Byte] =
    new Int8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength).toArray

  def toByteArray(value: js.Any): Array[Byte] =
    if !JSInterop.isDefined(value) then Array.emptyByteArray
    else
      value match
        case bytes: Uint8Array => toByteArray(bytes)
        case _ =>
          val dyn = value.asInstanceOf[js.Dynamic]
          val buffer = dyn.selectDynamic("buffer")
          if JSInterop.isDefined(buffer) then
            val offset = JSInterop.intValue(dyn.selectDynamic("byteOffset").asInstanceOf[js.Any])
            val length = JSInterop.intValue(dyn.selectDynamic("byteLength").asInstanceOf[js.Any])
            toByteArray(new Uint8Array(buffer.asInstanceOf[ArrayBuffer], offset, length))
          else
            val length = JSInterop.intValue(dyn.selectDynamic("length").asInstanceOf[js.Any])
            val out = new Array[Byte](length)
            var i = 0
            while i < length do
              out(i) = JSInterop.intValue(dyn.apply(i).asInstanceOf[js.Any]).toByte
              i += 1
            out

  def uint8ArrayOrEmpty(value: js.Any): Uint8Array =
    if !JSInterop.isDefined(value) then new Uint8Array(0)
    else
      value match
        case bytes: Uint8Array => bytes
        case _ =>
          val dyn = value.asInstanceOf[js.Dynamic]
          val buffer = dyn.selectDynamic("buffer")
          if JSInterop.isDefined(buffer) then
            val offset = JSInterop.intValue(dyn.selectDynamic("byteOffset").asInstanceOf[js.Any])
            val length = JSInterop.intValue(dyn.selectDynamic("byteLength").asInstanceOf[js.Any])
            new Uint8Array(buffer.asInstanceOf[ArrayBuffer], offset, length)
          else new Uint8Array(0)
