package net.akouryy.hexd

import org.scalajs.jquery.{jQuery => Q, _}
import scala.scalajs.js.Dynamic.global

object HexagonyIDE {
  def main(args: Array[String]) {
    Q(() => {
      val a = global.location.hash.asInstanceOf[String] substring 1 split ","
      if(a.length == 2) {
        Q("#source .src-raw textarea") value global.atob(a(0)).asInstanceOf[String]
        Q("#run .stdin textarea") value global.atob(a(1)).asInstanceOf[String]
      }
      global.setInterval(() => {
        global.location.hash =
          global.btoa(Q("#source .src-raw textarea").value) + "," +
          global.btoa(Q("#run .stdin textarea").value)
      }, 1000)

      val hexMapView = new HexMapView(Q("#hexmap-container"), Q("#map-io"))

      val interpreterView = new InterpreterView(Q("#run"),
        Seq(hexMapView.onExecuted))

      val source = new SourceView(Q("#source"),
        Seq(hexMapView.onSourceChanged, interpreterView.onSourceChanged))
    })
  }
}

object Hexagony {
  val Directions = IndexedSeq[(Double, Double)](
    (-1, -0.5), (-1, 0.5), (0, 1), (1, 0.5), (1, -0.5), (0, -1)
  )

  val SlashDirection = IndexedSeq(2, 1, 0, 5, 4, 3)
  val BSlashDirection = IndexedSeq(0, 5, 4, 3, 2, 1)
  val LessDirection = IndexedSeq(Seq(5), Seq(4), Seq(1, 3), Seq(0), Seq(5), Seq(2))
  val GreaterDirection = 0 to 5 map { d => LessDirection(reversedDirection(d)) map reversedDirection }
  val UnderscoreDirection = IndexedSeq(4, 3, 2, 1, 0, 5)
  val BarDirection = IndexedSeq(1, 0, 5, 4, 3, 2)

  def nextDirectionPossible(c: Char, from: Int): Seq[Int] = c match {
    case '/' => Seq(SlashDirection(from))
    case '\\'=> Seq(BSlashDirection(from))
    case '<' => LessDirection(from)
    case '>' => GreaterDirection(from)
    case '_' => Seq(UnderscoreDirection(from))
    case '|' => Seq(BarDirection(from))
    case '@' | '[' | ']' | '#' => Seq()
    case _ => Seq(from)
  }
  def reversedDirection(from: Int) = (from + 3) % 6
}
