package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2017.Day22.OffsetPos
import Day9._
import eu.sim642.adventofcodelib.box.Box

import scala.annotation.tailrec

object Day11 {

  case class PaintState(pos: Pos, offset: Pos, paint: Map[Pos, Long]) {
    def posPaint: Long = paint(pos)
  }

  def runPaint(program: Memory, initialPaint: Map[Pos, Long]): PaintState = {
    @tailrec
    def helper(programState: ProgramState, paintState: PaintState): PaintState = {
      val newProgramState = programState.copy(inputs = LazyList.continually(paintState.posPaint))

      newProgramState.outputStates.take(2) match {
        case LazyList() => // halted
          paintState
        case LazyList((_, outPaint), (newProgramState, outDirection)) =>
          val newPaint = paintState.paint + (paintState.pos -> outPaint)
          val newOffset = outDirection match {
            case 0 => paintState.offset.left
            case 1 => paintState.offset.right
          }
          val newPos = paintState.pos + newOffset
          val newPaintState = PaintState(newPos, newOffset, newPaint)
          helper(newProgramState, newPaintState)
      }
    }

    helper(ProgramState(program), PaintState(Pos.zero, Pos(0, 1), initialPaint))
  }

  def countPainted(program: Memory): Int = runPaint(program, Map.empty.withDefaultValue(0L)).paint.size

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
    val paint = runPaint(program, Map(Pos.zero -> 1L).withDefaultValue(0L)).paint
    printPaint(paint)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPainted(parseProgram(input)))
    renderIdentifier(parseProgram(input)) // BLULZJLZ
  }
}
