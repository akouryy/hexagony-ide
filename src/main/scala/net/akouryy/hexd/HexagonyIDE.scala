package net.akouryy.hexd
import org.scalajs.jquery.{jQuery => Q, _}

object HexagonyIDE {
  def main(args: Array[String]) {
    Q(() => {
      val source = new SourceView(Q("#source"))
    })
  }
}
