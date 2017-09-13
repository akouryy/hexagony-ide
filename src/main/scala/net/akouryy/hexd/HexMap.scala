package net.akouryy.hexd
import scala.scalajs.js
import js.{Dictionary => Dict}
import org.scalajs.jquery.{jQuery => Q, _}

class HexMapView(val q: JQuery) {
  private[this] val qSVG = q find "svg.hexmap"

  var source: Source = _ // initialized in onSourceChanged (by SourceView#updateSource)
  var hexMap: HexMap = _

  def onSourceChanged(s: Source) {
    source = s
    hexMap = new HexMap(source)

    qSVG(0) setAttribute("viewBox", s"0 0 ${source.size * 100} ${source.size * 84}")
    qSVG attr("width", source.size * 100) attr("height", source.size * 84)

    qSVG.empty()

    0 until source.size*2-1 foreach { j =>
      val sp = source.rowSpaceLengths(j)
      source.hexRows(j).zipWithIndex foreach { case (c, i) =>
        val x = 25 * (sp + i * 2 + 1)
        val y = 42 * j + 21
        val qChar = Q("<text>") text c.toString attr Dict(
          "x" -> x, "y" -> y,
        ) addClass "source-char"
        val qCharBG = Q("<circle>") attr Dict(
          "cx" -> x, "cy" -> y, "r" -> 10
        ) addClass "source-char-bg"
        hexMap.roadPassed(j)(i) zip Hexagony.Directions foreach { case (p, (dy, dx)) =>
          Q("<line>") attr Dict(
            "x1" -> x, "y1" -> y,
            "x2" -> (x + 25 * dx), "y2" -> (y + 21 * dy),
          ) css Dict(
            "stroke" -> (if(p) "rgba(255, 0, 0, 0.2)" else "rgba(0, 0, 0, 0.05)")
          ) addClass "road" appendTo qSVG
        }
        qCharBG appendTo qSVG
        qChar appendTo qSVG
      }
    }

    qSVG html qSVG.html
  }
}

class HexMap(val source: Source) {
  val rand = (js.Math.random() * 6).toInt
  val roadPassed: IndexedSeq[IndexedSeq[IndexedSeq[Boolean]]] = source.hexRows map {
    _ map { c =>
      val next = Hexagony.nextDirectionPossible(c, rand)
      0 to 5 map next.contains
    }
  }
}
