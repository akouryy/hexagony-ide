package net.akouryy.hexd

import scala.scalajs.js
import js.{Dictionary => Dict}
import org.scalajs.jquery.{jQuery => Q, _}

import shapeless._
import syntax.std.tuple._
import ops.hlist.Mapper

class SVGs(private[this] val size: Int, private[this] val cellWidth: RX[Double]) {
  val cellHeight = (cellWidth * Math.sqrt(3) / 2).axis[Ys]

  def realCoord(y: VY[Double], x: VX[Double]): (VY[Double], VX[Double]) = (
    VY(0) + (DY(size - 0.5) + (y - VY(0))) * cellHeight,
    VX(0) + (DX(size - 0.5) + (x - VX(0)) * 0.5) * cellWidth,
  )
  def realCoord(p: (VY[Double], VX[Double])): (VY[Double], VX[Double]) = realCoord(p._1, p._2)

  def qLine(y1: VY[Double], x1: VX[Double], y2: VY[Double], x2: VX[Double]): JQuery = {
    val (VY(y1r), VX(x1r)) = realCoord(y1, x1)
    val (VY(y2r), VX(x2r)) = realCoord(y2, x2)
    Q("<line>") attr Dict(
      "x1" -> x1r, "y1" -> y1r, "x2" -> x2r, "y2" -> y2r,
    )
  }
  def qLineD(y1: VY[Double], x1: VX[Double], dy: DY[Double], dx: DX[Double]) =
    qLine(y1, x1, y1 + dy, x1 + dx)

  def qCircle(cy: VY[Double], cx: VX[Double], r: Double) = {
    val (VY(cyr), VX(cxr)) = realCoord(cy, cx)
    Q("<circle>") attr Dict(
      "cy" -> cyr, "cx" -> cxr, "r" -> r
    )
  }

  def qText(y: VY[Double], x: VX[Double], txt: String) = {
    val (VY(yr), VX(xr)) = realCoord(y, x)
    Q("<text>") text txt attr Dict(
      "y" -> yr, "x" -> xr
    ) addClass "raw-char"
  }

  def qPath(d: (Char, Seq[(VY[Double], VX[Double])])*) = {
    Q("<path>") attr Dict(
      "d" -> d.map { case (c, v) =>
        c.toString + v.map { case (vy, vx) =>
          val (VY(y), VX(x)) = realCoord(vy, vx)
          x + "," + y
        }.mkString(" ")
      }.mkString
    )
  }
}
