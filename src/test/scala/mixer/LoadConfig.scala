package paqFe.mixer

import scala.io.Source
import java.util.regex.Pattern

object GetMixerConfig {
  def apply(file: String = "./paqFe/verify/db/mixer-config-small") = {
    val str = Source.fromFile(file).getLines().next()
    val p = Pattern.compile("[0-9]+")
    val m = p.matcher(str)
    
    m.find()
    val nFeatures = m.group().toInt
    m.find()
    val nHidden = m.group().toInt

    new MixerParameter(nFeatures, nHidden)
  }
}