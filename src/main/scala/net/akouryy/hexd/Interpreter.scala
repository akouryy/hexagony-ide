package net.akouryy.hexd

import scala.scalajs.js
import js.Dynamic.global
import org.scalajs.jquery.{jQuery => Q, _}

class InterpreterView(val q: JQuery) {
  private[this] val qIn = q find ".stdin > textarea" on ("input", () => updateInterpreter())
  private[this] val qOut = q find ".stdout > textarea"

  q find ".step" on("click", (ev: JQueryEventObject) => {
    val running = 1 to Q(ev.target).data("step").asInstanceOf[Int] forall { _ => interpreter.next() }

    import js.JSConverters._
    global.console.log(interpreter.pos.toString, interpreter.mempos.toString, interpreter.memory(interpreter.mempos))

    if(!running) {
      global.alert("execution done.");
    }
  })

  var source: Source = _ // initialized in onSourceChanged (by SourceView#updateSource)
  var interpreter: Interpreter = _

  def onSourceChanged(s: Source) {
    source = s
    updateInterpreter()
  }

  def updateInterpreter() {
    qOut value ""
    interpreter = new Interpreter(source, qIn.value.asInstanceOf[String],
      s => qOut value (qOut.value.asInstanceOf[String] + s))
  }
}

class Interpreter(
  val source: Source,
  input: String,
  val output: String => Unit,
  val memory: Memory = new Memory(),
  var pos: IP = null,
  var mempos: MemoryPosition = MemoryPosition(0, 0, 0),
) {
  pos = IP(1 - source.size, (1 - source.size) / 2.0, 2, source.size)


  var inputList: List[Char] = input.toList

  def next(): Boolean = {
    source(pos.y, pos.x) match {
      case ')' => memory(mempos) += 1
      case '(' => memory(mempos) -= 1
      case '+' => memory(mempos) = memory(mempos.left) + memory(mempos.right)
      case '-' => memory(mempos) = memory(mempos.left) - memory(mempos.right)
      case '*' => memory(mempos) = memory(mempos.left) * memory(mempos.right)
      case ':' => memory(mempos) = memory(mempos.left) / memory(mempos.right)
      case '%' => memory(mempos) = memory(mempos.left) % memory(mempos.right)
      case '~' => memory(mempos) *= -1

      case ',' =>
        memory(mempos) = inputList.headOption map (_.toLong) getOrElse -1
        if(!inputList.isEmpty) inputList = inputList.tail
      case '?' =>
        val (s, in) = inputList dropWhile { c => !('0' to '9' union "+-" contains c) } match {
          case Nil => (0, Nil)
          case '+' :: in => (1, in)
          case '-' :: in => (-1, in)
          case in => (1, in)
        }
        in span { '0' to '9' contains _ } match {
          case (Nil, i) =>
            memory(mempos) = 0
            inputList = i
          case (n, i) =>
            memory(mempos) = s * n.mkString.toLong
            inputList = i
        }

      case ';' => output((memory(mempos) % 256).toChar.toString)
      case '!' => output(memory(mempos).toString)
      case '{' => mempos = mempos.left
      case '}' => mempos = mempos.right
      case '"' => mempos = mempos.rev.right.rev
      case '\''=> mempos = mempos.rev.left.rev
      case '=' => mempos = mempos.rev
      case '^' => mempos = if(memory(mempos) > 0) mempos.right else mempos.left
      case '&' => memory(mempos) = memory(if(memory(mempos) > 0) mempos.right else mempos.left)

      case c if '0' to '9' contains c =>
        memory(mempos) = memory(mempos) * 10 + (c - '0')
      case c if ('a' to 'z') ++ ('A' to 'Z') contains c =>
        memory(mempos) = c

      case '@' => return false
      case _ =>
    }

    val positive = memory(mempos) > 0

    pos = source(pos.y, pos.x) match {
      case '$' => pos.step(positive).step(positive)
      case '_' => pos.copy(d = Hexagony.UnderscoreDirection(pos.d)).step(positive)
      case '|' => pos.copy(d = Hexagony.BarDirection       (pos.d)).step(positive)
      case '/' => pos.copy(d = Hexagony.SlashDirection     (pos.d)).step(positive)
      case '\\'=> pos.copy(d = Hexagony.BSlashDirection    (pos.d)).step(positive)
      case '<' => (Hexagony.LessDirection(pos.d): @unchecked) match {
        case Seq(d) => pos.copy(d = d).step(positive)
        case Seq(l, r) => pos.copy(d = if(memory(mempos) > 0) r else l).step(positive)
      }
      case '>' => (Hexagony.GreaterDirection(pos.d): @unchecked) match {
        case Seq(d) => pos.copy(d = d).step(positive)
        case Seq(l, r) => pos.copy(d = if(memory(mempos) > 0) r else l).step(positive)
      }
      case _ => pos.step(positive)
    }

    true
  }
}

case class IP(y: Int, x: Double, d: Int, size: Int) {
  def step(positive: Boolean) = {
    val s1 = size - 1

    val yn = y + IP.DY(d)
    val xn = x + IP.DX(d)
    val (yt, xt): (Int, Double) = (
      x == -s1 - y * 0.5, y == -s1, x ==  s1 + y * 0.5,
      x ==  s1 - y * 0.5, y ==  s1, x == -s1 + y * 0.5, d
    ) match {
      case (true, true, _, _, _, _, 0) => IP.edge(s1, if(positive) 2 else 4)
      case (_, true, true, _, _, _, 1) => IP.edge(s1, if(positive) 3 else 5)
      case (_, _, true, true, _, _, 2) => IP.edge(s1, if(positive) 4 else 0)
      case (_, _, _, true, true, _, 3) => IP.edge(s1, if(positive) 5 else 1)
      case (_, _, _, _, true, true, 4) => IP.edge(s1, if(positive) 0 else 2)
      case (true, _, _, _, _, true, 5) => IP.edge(s1, if(positive) 1 else 3)
      case (true, _, _, _, _, _, 5 | 0) => (y + s1, x + s1 * 1.5)
      case (_, true, _, _, _, _, 0 | 1) => (y + s1 * 2, x)
      case (_, _, true, _, _, _, 1 | 2) => (y + s1, x - s1 * 1.5)
      case (_, _, _, true, _, _, 2 | 3) => (y - s1, x - s1 * 1.5)
      case (_, _, _, _, true, _, 3 | 4) => (y - s1 * 2, x)
      case (_, _, _, _, _, true, 4 | 5) => (y - s1, x + s1 * 1.5)
      case _ => (yn, xn)
    }
    IP(yt, xt, d, size)
  }

  def left  = IP(y, x, (d - 1) % 6, size)
  def right = IP(y, x, (d + 1) % 6, size)
}

object IP {
  val DY = IndexedSeq(-1, -1, 0, 1, 1, 0)
  val DX = IndexedSeq(-0.5, 0.5, 1, 0.5, -0.5, -1)

  def edge(s1: Int, d: Int): (Int, Double) = d match {
    case 0 => (-s1, s1 * -0.5)
    case 1 => (-s1, s1 * 0.5)
    case 2 => (0, s1)
    case 3 => (s1, s1 * 0.5)
    case 4 => (s1, s1 * -0.5)
    case 5 => (0, -s1)
  }
}
