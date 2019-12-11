package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2017.Day22.OffsetPos
import Day9._
import eu.sim642.adventofcodelib.box.Box

import scala.annotation.tailrec

object Day11 {

  case class PaintState(pos: Pos, offset: Pos, paint: Map[Pos, Long])

  def runPaint(program: Memory, initialPaint: Map[Pos, Long] = Map.empty): PaintState = {
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

    helper(ProgramState(program), PaintState(Pos.zero, Pos(0, 1), initialPaint.withDefaultValue(0)))
  }

  def countPainted(program: Memory): Int = runPaint(program).paint.size

  def printPaint(paint: Map[Pos, Long]): Unit = {
    val Box(min, max) = Box.bounding(paint.keys)
    for (y <- (min.y to max.y).reverse) {
      for (x <- min.x to max.x)
        print(paint(Pos(x, y)) match {
          case 0 => '.'
          case 1 => '#'
        })
      println()
    }
  }

  def renderIdentifier(program: Memory): Unit = {
    val paint = runPaint(program, Map(Pos.zero -> 1)).paint
    printPaint(paint)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPainted(parseProgram(input)))
    renderIdentifier(parseProgram(input)) // BLULZJLZ
  }
}
