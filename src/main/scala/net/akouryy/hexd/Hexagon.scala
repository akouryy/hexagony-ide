package net.akouryy.hexd

import shapeless._
import tag.@@

object Hexagon {
  def rowSizes(size: Int): IndexedSeq[Int] =
    (size until size * 2) ++ (size * 2 - 2 to size by -1)

  def rowLeftPaddings(size: Int): IndexedSeq[Int] =
    rowSizes(size) map (size * 2 - 1 - _)

  def coordToIndex(y: VY[Int], x: VX[Int], size: Int): (Int, Int) = {
    val j = y.v + size - 1
    (j, (x.v + rowSizes(size)(j) - 1) / 2)
  }

  def indexToCoord(j: Int, i: Int, size: Int): (VY[Int], VX[Int]) = {
    (VY(j - size + 1), VX(i * 2 - rowSizes(size)(j) + 1))
  }

  val dyRaw = IndexedSeq(DY(-1), DY(-1), DY(0), DY(1), DY(1), DY(0))
  val dxRaw = IndexedSeq(DX(-1), DX(1), DX(2), DX(1), DX(-1), DX(-2))

  def dy(d: Direction) = dyRaw(d.dir)
  def dx(d: Direction) = dxRaw(d.dir)

  def corner(size: Int, d: Direction): (VY[Int], VX[Int]) =
    (VY(0) + Hexagon.dy(d) * (size - 1), VX(0) + Hexagon.dx(d) * (size - 1))

  def cellCoordinates(size: Int): Seq[(VY[Int], VX[Int])] =
    rowSizes(size).zipWithIndex flatMap { case (r, j) =>
      0 until r map (i => (VY(j), VX(i)))
    }
}

case class Direction(dir: Int) extends AnyVal {
  // assert(0 <= dir && dir < 6)

  def +(e: Int) = {
    val f = (dir + e) % 6
    Direction(if(f >= 0) f else f + 6)
  }
  def -(e: Int) = this + -e

  @inline private[this] def reduced1(e: Int) = if(e < 6) e else e - 6
  @inline private[this] def reducedDirection1(e: Int) = Direction(reduced1(e))

  def rev = reducedDirection1(dir + 3)

  def | = reducedDirection1(7 - dir)
  def `_` = reducedDirection1(10 - dir)
  def / = reducedDirection1(8 - dir)
  def \ = reducedDirection1(6 - dir)

  def <(positive: Boolean @@ IP) = Direction(dir match {
    case 0 | 4 => 5
    case 1 | 3 | 5 => reduced1(dir + 3)
    case 2 => if(positive) 3 else 1
  })
  def >(positive: Boolean @@ IP) = Direction(dir match {
    case 1 | 3 => 2
    case 0 | 2 | 4 => reduced1(dir + 3)
    case 5 => if(positive) 0 else 4
  })
}
