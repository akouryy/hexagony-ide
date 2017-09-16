package net.akouryy.hexd

import collection.mutable

import scala.scalajs.js
import js.{Dictionary => Dict}
import js.Dynamic.global
import org.scalajs.jquery.{jQuery => Q, _}

import shapeless._
import syntax.std.tuple._
import tag.@@
import ops.hlist._


class HexMapView(
  private[this] val q: JQuery,
  private[this] val qi: JQuery,
  private[this] val cellWidth: Double
) {
  private[this] val cellHeight: Double = cellWidth * Math.sqrt(3) / 2
  private[this] val circleSize: Double = cellWidth / 5
  private[this] val circleX: DX[Double] = DX(10 / cellWidth * 2)
  private[this] val circleY: DY[Double] = DY(10 / cellHeight)

  private[this] var size: Int = _ // tag[Source].apply(source.size)
  private[this] var diagonalSize: Int = _ // size * 2 - 1
  private[this] var svgs: SVGs = _ // new SVGs(size, cellWidth)

  private[this] val qSVG = q find "svg.hexmap"

  private[this] var source: Source = _ // initialized in onSourceChanged (by SourceView#updateSource)
  private[this] var hexMap: HexMap = _

  def onSourceChanged(s: Source) {
    source = s
    size = tag[Source].apply(source.size)
    diagonalSize = size * 2 - 1
    svgs = new SVGs(size, cellWidth)

    hexMap = new HexMap(source)

    val height = diagonalSize * cellHeight
    val width = diagonalSize * cellWidth
    qSVG(0) setAttribute("viewBox", s"0 0 $width $height")
    qSVG attr Dict("width" -> width, "height" -> height)

    qSVG.empty()

    0 until diagonalSize foreach { j =>
      val sp = source.rowSpaceLengths(j)
      source.hexRows(j).zipWithIndex foreach { case (c, i) =>
        val (y, x) = Hexagon.indexToCoord(j, i, size)
        val (VY(yr), VX(xr)) = svgs.realCoord(y, x)

        Hexagon.dyRaw.zip(Hexagon.dxRaw).zipWithIndex foreach { case ((dy, dx), d) =>
          svgs.qLineD(y, x, dy / 2.0, dx / 2.0) addClass s"road road-${j}-${i}-${d} uncolored" appendTo qSVG
        }

        val qChar = c match {
          case '_' =>
            svgs.qLine(y, x - circleX, y, x + circleX) addClass "source-mirror"
          case '|' =>
            svgs.qLine(y - circleY, x, y + circleY, x) addClass "source-mirror"
          case '/' | '\\' =>
            val s = RY(if(c == '/') +1 else -1)
            svgs.qLine(
              y + s * circleY * Math.sqrt(3) / 2, x - circleX * 0.5,
              y - s * circleY * Math.sqrt(3) / 2, x + circleX * 0.5,
            ) addClass "source-mirror"
          case '<' | '>' =>
            val s = RX(if(c == '<') +1 else -1)
            svgs.qPath(
              'M' -> Seq((y - circleY * Math.sqrt(3) / 2, x + s * circleX / 2)),
              'L' -> Seq((y,                              x - s * circleX)),
              'L' -> Seq((y + circleY * Math.sqrt(3) / 2, x + s * circleX / 2)),
            ) addClass "source-mirror"

          case '[' | ']' | '#' =>
            global.alert("multiple IPs are not supported now.")
            svgs.qCircle(y, x, circleSize) addClass "raw-char-error"
          case _ =>
            Q("<g>") append (
              svgs.qCircle(y, x, circleSize) addClass "raw-char-bg",
              svgs.qText(y, x, c.toString) addClass "raw-char"
            )
        }

        qChar addClass s"char char-${j}-${i}" appendTo qSVG
        if(j == 0 && i == 0) qChar addClass "current"
      }
    }

    qSVG html qSVG.html
  }

  def onExecuted(passed: List[IP]) {
    passed.reverseIterator foreach { case IP(y, x, dir, _) =>
      val (j, i) = Hexagon.coordToIndex(y, x, size)
      val d = dir.dir
      hexMap.visitCount(j)(i)(d) += 1
      hexMap.firstVisit(j)(i)(d) = hexMap.firstVisit(j)(i)(d) orElse {
        hexMap.firstVisitMax += 1
        Some(hexMap.firstVisitMax)
      }
    }
    passed.headOption foreach { case IP(y, x, _, _) =>
      val (j, i) = Hexagon.coordToIndex(y, x, size)
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
