package verifydata

import java.io._
import scala.io.Source

class VerifyData(file : String) {
  val source = Source.fromFile(file)
  val data = source.getLines().map(_.split(",")).toArray
  source.close
}

class ByteStream(pathname : String) {
  private val is = new FileInputStream(pathname)
  private val buf = new Array[Byte](1024)

  private var n = 0
  private var idx = 0

  private var last = false
  def getByte() : (Byte, Boolean) = {
    val bufEnd = idx == n
    val streamEnd = is.available() == 0
    
    if(bufEnd && !streamEnd) {
      n = is.read(buf)
      idx = 0
    }

    if(!last) {
      idx = idx + 1
      last = streamEnd && idx == n
    }
    
    (buf(idx - 1), last)
  }

  def end() = idx == n && is.available() == 0
}