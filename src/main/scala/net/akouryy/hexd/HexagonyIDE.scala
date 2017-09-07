package net.akouryy.hexd
import org.scalajs.jquery.{jQuery => Q, _}

object HexagonyIDE {
  def main(args: Array[String]) {
    Q(() => {
      val hexMapView = new HexMapView(Q("#hexmap-container"))

      val source = new SourceView(Q("#source"), Seq(hexMapView.onSourceChanged))
    })
  }
}

object Hexagony {
  val Directions = IndexedSeq[(Double, Double)](
    (-1, -0.5), (-1, 0.5), (0, 1), (1, 0.5), (1, -0.5), (0, -1)
  )

  private[this] val SlashDirection = IndexedSeq(2, 1, 0, 5, 4, 3)
  private[this] val BSlashDirection = IndexedSeq(0, 5, 4, 3, 2, 1)
  private[this] val LessDirection = IndexedSeq(Seq(5), Seq(4), Seq(1, 3), Seq(0), Seq(5), Seq(2))
  private[this] val UnderscoreDirection = IndexedSeq(4, 3, 2, 1, 0, 5)
  private[this] val BarDirection = IndexedSeq(1, 0, 5, 4, 3, 2)

  def nextDirectionPossible(c: Char, from: Int): Seq[Int] = c match {
    case '/' => Seq(SlashDirection(from))
    case '\\'=> Seq(BSlashDirection(from))
    case '<' => LessDirection(from)
    case '>' => LessDirection(reversedDirection(from)) map reversedDirection
    case '_' => Seq(UnderscoreDirection(from))
    case '|' => Seq(BarDirection(from))
    case '@' | '[' | ']' | '#' => Seq()
    case _ => Seq(from)
  }
  def reversedDirection(from: Int) = (from + 3) % 6
}
