package net.akouryy.hexd

import collection.mutable
import scala.scalajs.js
import js.{Dictionary => Dict}
import js.Dynamic.global
import org.scalajs.jquery.{jQuery => Q, _}

class HexMapView(val q: JQuery, val qi: JQuery) {
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

        Hexagony.Directions.zipWithIndex foreach { case ((dy, dx), d) =>
          Q("<line>") attr Dict(
            "x1" -> x, "y1" -> y,
            "x2" -> (x + 25 * dx), "y2" -> (y + 21 * dy),
          ) addClass s"road road-${j}-${i}-${d} uncolored" appendTo qSVG
        }

        val qChar = c match {
          case '_' =>
            Q("<line>") attr Dict(
              "x1" -> (x - 10), "y1" -> y, "x2" -> (x + 10), "y2" -> y
            ) addClass "source-mirror"
          case '|' =>
            Q("<line>") attr Dict(
              "x1" -> x, "y1" -> (y - 10), "x2" -> x, "y2" -> (y + 10)
            ) addClass "source-mirror"
          case '/' | '\\' =>
            val s = if(c == '/') +1 else -1
            Q("<line>") attr Dict(
              "x1" -> (x - 5), "y1" -> (y + 8 * s), "x2" -> (x + 5), "y2" -> (y - 8 * s)
            ) addClass "source-mirror"
          case '<' | '>' =>
            val s = if(c == '<') +1 else -1
            Q("<path>") attr Dict(
              "d" -> s"M ${x + 5 * s} ${y - 8} L ${x - 10 * s} ${y} L ${x + 5 * s} ${y + 8}"
            ) addClass "source-mirror"

          case '[' | ']' | '#' =>
            global.alert("multiple IPs are not supported now.")
            Q("<circle>") attr Dict(
              "cx" -> x, "cy" -> y, "r" -> 10
            ) addClass "raw-char-error"
          case _ =>
            Q("<g>") append (
              Q("<circle>") attr Dict(
                "cx" -> x, "cy" -> y, "r" -> 10
              ) addClass "raw-char-bg"
            ,
              Q("<text>") text c.toString attr Dict(
                "x" -> x, "y" -> y,
              ) addClass "raw-char"
            )
        }

        qChar addClass s"char char-${j}-${i}" appendTo qSVG
        if(j == 0 && i == 0) qChar addClass "current"
      }
    }

    qSVG html qSVG.html
  }

  def onExecuted(passed: List[(Int, Int, Int)]) {
    passed.reverseIterator foreach { case (j, i, d) =>
      hexMap.visitCount(j)(i)(d) += 1
      hexMap.firstVisit(j)(i)(d) = hexMap.firstVisit(j)(i)(d) orElse {
        hexMap.firstVisitMax += 1
        Some(hexMap.firstVisitMax)
      }
    }
    passed.headOption foreach { case (j, i, d) =>
      qSVG find ".char" removeClass "current"
      qSVG find s".char-${j}-${i}" addClass "current"
    }
    updateRoad()
  }

  var colorFn: (Int, Int, Int) => Option[String] = {
    val visitCount: (Int, Int, Int) => Option[String] = { (j, i, d) =>
      val e = hexMap.visitCount(j)(i)(d)
      if(e == 0)
        None
      else
        Some(s"hsl(${Math.log(e) * 270.0 / hexMap.logmaxVisitCount}, 100%, 50%)")
    }
    val firstVisit: (Int, Int, Int) => Option[String] = { (j, i, d) =>
      hexMap.firstVisit(j)(i)(d) map { e =>
        s"hsl(${e * 270.0 / hexMap.firstVisitMax}, 100%, 50%)"
      }
    }
    qi find ".colorfn-visit-count" click { () => colorFn = visitCount; updateRoad() }
    qi find ".colorfn-first-visit" click { () => colorFn = firstVisit; updateRoad() }

    visitCount
  }

  def updateRoad() {
    hexMap.foreachEdge { (j, i, d) =>
      colorFn(j, i, d) match {
        case Some(c) =>
          qSVG find s".road-${j}-${i}-${d}" attr Dict(
            "stroke" -> c,
          ) removeClass "uncolored"
        case _ =>
          qSVG find s".road-${j}-${i}-${d}" addClass "uncolored"
      }
    }
    qSVG html qSVG.html
  }
}

class HexMap(val source: Source) {
  def foreachEdge(fn: (Int, Int, Int) => Unit) {
    0 until source.size * 2 - 1 foreach { j =>
      0 until source.rowSizes(j) foreach { i =>
        0 until 6 foreach { fn(j, i, _) }
      }
    }
  }

  val visitCount: IndexedSeq[IndexedSeq[mutable.IndexedSeq[Int]]] =
    IndexedSeq.tabulate(source.size * 2 - 1)(j =>
      IndexedSeq.fill(source.rowSizes(j))(
        mutable.IndexedSeq.fill(6)(0)
      )
    )
  def logmaxVisitCount: Double = Math.log(visitCount.map(_.map(_.max).max).max + 1)

  val firstVisit: IndexedSeq[IndexedSeq[mutable.IndexedSeq[Option[Int]]]] =
    IndexedSeq.tabulate(source.size * 2 - 1)(j =>
      IndexedSeq.fill(source.rowSizes(j))(
        mutable.IndexedSeq.fill(6)(None)
      )
    )
  var firstVisitMax = 0
}
