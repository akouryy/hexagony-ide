package net.akouryy.hexd

import collection.mutable

class Memory {
  private[this] val memory = mutable.HashMap[(Int, Int, Int), Long]()
  def apply(p: MemoryPosition): Long = memory.getOrElse((p.y, p.x, p.d % 3), 0)
  def update(p: MemoryPosition, v: Long) { memory((p.y, p.x, p.d % 3)) = v }
}

case class MemoryPosition(y: Int, x: Int, d: Int) {
  def rev = copy(d = (d + 3) % 6)

  def right = d match {
    case 0 => MemoryPosition(y,   x,   1)
    case 1 => MemoryPosition(y,   x,   2)
    case 2 => MemoryPosition(y-1, x+1, 3)
    case 3 => MemoryPosition(y+2, x,   4)
    case 4 => MemoryPosition(y+1, x-1, 5)
    case 5 => MemoryPosition(y-2, x,   0)
  }

  def left = d match {
    case 0 => MemoryPosition(y+1, x-1, 5)
    case 1 => MemoryPosition(y-2, x,   0)
    case 2 => MemoryPosition(y-1, x+1, 1)
    case 3 => MemoryPosition(y+2, x,   2)
    case 4 => MemoryPosition(y,   x,   3)
    case 5 => MemoryPosition(y,   x,   4)
  }
}
