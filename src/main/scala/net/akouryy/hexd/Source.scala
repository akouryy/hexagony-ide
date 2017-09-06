package net.akouryy.hexd
import scala.scalajs.js.timers
import org.scalajs.jquery.{jQuery => Q, _}

class SourceView(val q: JQuery) {
  private[this] val qRaw = q find ".src-raw > textarea"
  private[this] val qMin = q find ".src-min > textarea"
  private[this] val qHex = q find ".src-hex > textarea"

  var source: Source = _ // initialized in updateSource()

  private[this] def updateSource() {
    source = new Source(qRaw.value.asInstanceOf[String], None)
    qMin value source.min
    qHex value source.hex
  }
  updateSource()

  private[this] var updateSourceHandle = None: Option[timers.SetTimeoutHandle]
  qRaw.on("input", () => {
    updateSourceHandle foreach timers.clearTimeout
    updateSourceHandle = Some(timers.setTimeout(500)(updateSource))
  })
}

class Source(val raw: String, sizeP: Option[Int]) {
  val (min, size) = {
    val packed = Source.toPack replaceAllIn(raw, "")
    val size = (sizeP ++ Some(Source.minSize(packed.length))).max
    val trimmed = Source.toTrim replaceFirstIn(packed, "")
    (trimmed padTo(Source.minLength(size), '.'), size)
  }

  val hex = {
    val rowSizes = (size until size*2) ++ (size*2-2 to size by -1)
    val rowIndices = rowSizes.scan(0)(_ + _) sliding 2
    val max = min padTo(Source.maxLength(size), '.')
    rowSizes zip(rowIndices.toIterable) map { case (s, Seq(i, j)) =>
      max slice(i, j) mkString(" " * (size * 2 - 1 - s), " ", "")
    } mkString "\n"
  }
}

object Source {
  private val toPack = """$\s+|\s|`""".r
  private val toTrim = """\.+$""".r

  def minSize(len: Int): Int =
    if(len > 0) (Math.sqrt(0.25 + (len - 1.0) / 3) + 0.5).ceil.toInt else 0

  def minLength(size: Int): Int =
    if(size > 1) 2 + (size - 1) * (size - 2) * 3 else size

  def maxLength(size: Int): Int = 1 + size * (size - 1) * 3
}
