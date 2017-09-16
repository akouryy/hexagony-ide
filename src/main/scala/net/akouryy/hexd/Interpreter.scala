package net.akouryy.hexd

import collection.mutable

import shapeless.{:: => #:, _}
import syntax.std.tuple._
import tag.@@

import scala.scalajs.js
import js.Dynamic.global
import org.scalajs.jquery.{jQuery => Q, _}

class InterpreterView(val q: JQuery, onExecutedEvent0: Seq[List[IP] => Unit] = Seq()) {
  private[this] val qIn = q find ".stdin > textarea" on ("input", () => updateInterpreter())
  private[this] val qOut = q find ".stdout > textarea"

  q find ".step" on("click", (ev: JQueryEventObject) => {
    val (passed, running) = interpreter step Q(ev.target).data("step").asInstanceOf[Int]

    onExecutedEvent foreach (_ apply passed)

    if(!running) {
      global.alert("execution done.");
    }
  })

  var source: Source = _ // initialized in onSourceChanged (by SourceView#updateSource)
  var interpreter: Interpreter = _
  val onExecutedEvent = mutable.Set[List[IP] => Unit](onExecutedEvent0: _*)

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
  private[this] val source: Source,
  input: String,
  private[this] val output: String => Unit,
) {
  private[this] var pos: IP = IP(VY(1 - source.size), VX(1 - source.size), Direction(2), source.size)
  private[this] val memory: Memory = new Memory()
  private[this] var mempos: MemoryPosition = MemoryPosition(VY(0), VX(0), Direction(0))

  private[this] var inputList: List[Char] = input.toList

  def next(): Option[List[IP]] = {
    val ch = source(pos.y, pos.x)

    ch match {
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

      case '@' => return None
      case _ =>
    }

    implicit val positive = tag[IP].apply(memory(mempos) > 0)

    val dn = ch match {
      case '_' => pos.d.`_`
      case '|' => pos.d.|
      case '/' => pos.d./
      case '\\'=> pos.d.\
      case '<' => pos.d.<(positive)
      case '>' => pos.d.>(positive)
      case _ => pos.d
    }

    val outPos = pos.copy(d = dn)

    pos = if(ch == '$') outPos.step.step else outPos.step

    Some(List(pos.copy(d = pos.d + 3), outPos))
  }

  def step(s: Int) = {
    val res = ((List[IP](), true) /: (1 to s)) { (g, _) =>
      g match {
        case (p, false) => (p, false)
        case (p, true) =>
          next() match {
            case Some(q) => (q ::: p, true)
            case None => (p, false)
          }
      }
    }
    global.console.log(pos.toString, mempos.toString, memory(mempos))
    res
  }
}

case class IP(y: VY[Int], x: VX[Int], d: Direction, size: Int) {
  def step(implicit positive: IP.Positive) = {
    val s1 = size - 1
    val sx = DX(s1)
    val sy = DY(s1)

    val boundaryConds = Seq(
      x.v == -(y + sy * 2).v, y == VY(0) - sy, x.v == (y + sy * 2).v,
      x.v == -(y - sy * 2).v, y == VY(0) + sy, x.v == (y - sy * 2).v,
    )

    (IP.apply _).tupled((
      if(boundaryConds(d.dir) && boundaryConds((d + 1).dir)) {
        Hexagon.corner(size, if(positive) d + 2 else d + 4)
      } else if(boundaryConds(d.dir)) {
        (y + Hexagon.dx(d + 1).axis[Ys] * s1, x + Hexagon.dy(d + 1).axis[Xs] * -3 * s1)
      } else if(boundaryConds((d + 1).dir)) {
        (y + Hexagon.dx(d + 2).axis[Ys] * s1, x + Hexagon.dy(d + 2).axis[Xs] * -3 * s1)
      } else {
        (y + Hexagon.dy(d), x + Hexagon.dx(d))
      } 
    ) ++ (d, size))
  }
}

object IP {
  type Positive = Boolean @@ IP
}
