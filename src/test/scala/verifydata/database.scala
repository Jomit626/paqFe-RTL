package verifydata

import scala.io.Source
import org.scalatest.FlatSpec

class VerifyData(file : String) {
  val source = Source.fromFile(file)
  val data = source.getLines.map(_.split(",")).toArray
  source.close
}