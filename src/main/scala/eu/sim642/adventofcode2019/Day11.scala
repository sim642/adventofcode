package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2017.Day22.OffsetPos
import Day9._

import scala.annotation.tailrec

object Day11 {

  case class PaintState(pos: Pos, offset: Pos, paint: Map[Pos, Long])

  def countPainted(program: Memory): Int = {

    @tailrec
    def helper(programState: ProgramState, paintState: PaintState): PaintState = {
      val outputStates = programState.copy(inputs = LazyList.continually(paintState.paint(paintState.pos))).execs
        .filter(_._2.isDefined)
        .map(p => (p._1, p._2.get))

      outputStates.take(2) match {
        case LazyList() =>
          paintState
        case LazyList((_, out1), (newState, out2)) =>
          val newPaint = paintState.paint + (paintState.pos -> out1)
          val newOffset = out2 match {
            case 0 => paintState.offset.left
            case 1 => paintState.offset.right
          }
          val newPos = paintState.pos + newOffset
          //println(out1, out2)
          helper(newState, PaintState(newPos, newOffset, newPaint))
      }
    }

    helper(ProgramState(program), PaintState(Pos.zero, Pos(0, 1), Map.empty.withDefaultValue(0))).paint.size
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPainted(parseProgram(input)))
  }
}
