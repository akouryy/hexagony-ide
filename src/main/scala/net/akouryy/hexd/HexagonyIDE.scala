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

      val hexMapView = new HexMapView(Q("#hexmap-container"), Q("#map-io"), 50)

      val interpreterView = new InterpreterView(Q("#run"),
        Seq(hexMapView.onExecuted))

      val source = new SourceView(Q("#source"),
        Seq(hexMapView.onSourceChanged, interpreterView.onSourceChanged))
    })
  }
}
