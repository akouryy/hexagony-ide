package net.akouryy.hexd

import collection.mutable

import shapeless._
import syntax.std.tuple._

class Memory {
  private[this] val memory = mutable.HashMap[(VY[Int], VX[Int], Int), Long]()
  def apply(p: MemoryPosition): Long = memory.getOrElse((p.y, p.x, p.d.dir % 3), 0)
  def update(p: MemoryPosition, v: Long) { memory((p.y, p.x, p.d.dir % 3)) = v }
}

case class MemoryPosition(y: VY[Int], x: VX[Int], d: Direction) {
  def rev = copy(d = d + 3)

  def right = MemoryPosition.tupled((d.dir match {
    case 0 => (y, x)
    case 1 => (y, x)
    case 2 => (y - DY(1), x + DX(1))
    case 3 => (y + DY(2), x)
    case 4 => (y + DY(1), x - DX(1))
    case 5 => (y - DY(2), x)
  }) :+ d + 1)

  def left = MemoryPosition.tupled((d.dir match {
    case 0 => (y + DY(1), x - DX(1))
    case 1 => (y - DY(2), x)
    case 2 => (y - DY(1), x + DX(1))
    case 3 => (y + DY(2), x)
    case 4 => (y, x)
    case 5 => (y, x)
  }) :+ d - 1)
}
