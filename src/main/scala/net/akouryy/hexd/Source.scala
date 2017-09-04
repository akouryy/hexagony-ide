package net.akouryy.hexd
import scala.scalajs.js.timers
import org.scalajs.jquery.{jQuery => Q, _}

class SourceView(val q: JQuery) {
  private[this] val qRaw = q find ".src-raw > textarea"
  private[this] val qMin = q find ".src-min > textarea" `val` ""
  private[this] val qHex = q find ".src-hex > textarea" `val` ""

  var source: Source = _ // initialized in updateSource()

  private[this] def updateSource() {
    source = new Source(qRaw.`val`.asInstanceOf[String], None)
    qMin `val` source.min
  }
  updateSource()

  private[this] var updateSourceHandle = None: Option[timers.SetTimeoutHandle]
  qRaw.on("input", () => {
    updateSourceHandle.foreach(timers.clearTimeout)
    updateSourceHandle = Some(timers.setTimeout(500)(updateSource))
  })
}

class Source(val raw: String, sizeP: Option[Int]) {
  val (min, size) = Source.minify(raw, sizeP)
}

object Source {
  private[this] val toPack = """$\s+|\s|`""".r
  private[this] val toTrim = """\.+$""".r

  def minSize(len: Int): Int =
    if(len > 0) (Math.sqrt(0.25 + (len - 1.0) / 3) + 0.5).ceil.toInt else 0

  def minLength(size: Int): Int =
    if(size > 1) 2 + (size - 1) * (size - 2) * 3 else size

  def maxLength(size: Int): Int = 1 + size * (size - 1) * 3

  def minify(raw: String, sizeP: Option[Int]): (String, Int) = {
    val packed = toPack.replaceAllIn(raw, "")
    val size = (sizeP ++ Some(minSize(packed.length))).max
    val trimmed = toTrim.replaceFirstIn(packed, "")
    (trimmed.padTo(minLength(size), '.'), size)
  }
}
