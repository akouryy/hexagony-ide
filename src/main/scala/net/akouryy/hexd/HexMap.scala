package net.akouryy.hexd

import collection.mutable
import scala.scalajs.js
import js.{Dictionary => Dict}
import js.Dynamic.global
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
        Hexagony.Directions.zipWithIndex foreach { case ((dy, dx), d) =>
          Q("<line>") attr Dict(
            "x1" -> x, "y1" -> y,
            "x2" -> (x + 25 * dx), "y2" -> (y + 21 * dy),
          ) addClass s"road road-${j}-${i}-${d} unvisited" appendTo qSVG
        }
        qCharBG appendTo qSVG
        qChar appendTo qSVG
      }
    }

    qSVG html qSVG.html
  }

  def onExecuted(passed: List[(Int, Int, Int)]) {
    passed foreach { case (y, x, d) =>
      hexMap.roadPassed(y)(x)(d) += 1
    }
    updateRoad()
  }

  def updateRoad() {
    hexMap.roadPassed.zipWithIndex foreach { case (r, j) =>
      r.zipWithIndex foreach { case (c, i) =>
        c.zipWithIndex foreach { case (e, d) =>
          if(e != 0) {
            qSVG find s".road-${j}-${i}-${d}" attr Dict(
              "stroke" -> s"hsl(${Math.log(e + 1) * 270.0 / hexMap.logmax}, 100%, 50%)",
              "title" -> s"passed ${e} times",
            ) removeClass "unvisited"
          }
        }
      }
    }
    qSVG html qSVG.html
  }
}

class HexMap(val source: Source) {
  val roadPassed: IndexedSeq[IndexedSeq[mutable.IndexedSeq[Int]]] = source.hexRows map {
    _ map { _ => mutable.IndexedSeq.fill(6)(0) }
  }

  def logmax: Double = roadPassed.map(_.map(_.map(e => Math.log(e + 1)).max).max).max
}
