package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2017.Day22.OffsetPos
import intcode._
import eu.sim642.adventofcodelib.box.Box

import scala.annotation.tailrec

object Day11 {

  case class PaintState(pos: Pos, offset: Pos, paint: Map[Pos, Long]) {
    def posPaint: Long = paint(pos)
  }

  trait Solution {
    def runPaint(program: Memory, initialPaint: Map[Pos, Long]): PaintState

    def countPainted(program: Memory): Int = {
      runPaint(program, Map.empty.withDefaultValue(0L)).paint.size
    }

    def renderIdentifier(program: Memory): Unit = {
      val paint = runPaint(program, Map(Pos.zero -> 1L).withDefaultValue(0L)).paint
      printPaint(paint)
    }
  }

  /**
    * Solution which uses recursive LazyList knot tying to combine ProgramState and PaintState.
    *
    * Works for these tasks, but implicitly assumes each input corresponds exactly to two outputs.
    * Behaves incorrectly when program reads (same) input multiple times per two outputs
    * or doesn't read input at all per two outputs.
    */
  object KnotTyingSolution extends Solution {
    override def runPaint(program: Memory, initialPaint: Map[Pos, Value]): PaintState = {
      lazy val paintStates: LazyList[PaintState] = PaintState(Pos.zero, Pos(0, 1), initialPaint) #:: newPaintStates
      lazy val inputs: LazyList[Value] = paintStates.map(_.posPaint)
      lazy val outputs: LazyList[Value] = ProgramState(program, inputs).outputs
      lazy val newPaintStates: LazyList[PaintState] = (paintStates zip outputs.grouped(2)).map({
        case (PaintState(pos, offset, paint), LazyList(outPaint, outDirection)) =>
          val newPaint = paint + (pos -> outPaint)
          val newOffset = outDirection match {
            case 0 => offset.left
            case 1 => offset.right
          }
          val newPos = pos + newOffset
          PaintState(newPos, newOffset, newPaint)
        case (_, _) =>
          throw new IllegalArgumentException("incomplete output")
      })

      paintStates.last
    }
  }

  /**
    * Solution which uses tailrec loop to read program outputs two at a time.
    * Infinite (same) input list is given each time which may be read multiple times
    * or not read at all.
    *
    * Doesn't make the implicit assumption that KnotTyingSolution.
    */
  object OutputLoopSolution extends Solution {
    override def runPaint(program: Memory, initialPaint: Map[Pos, Value]): PaintState = {
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
  }

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

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import OutputLoopSolution._

    println(countPainted(parseProgram(input)))
    renderIdentifier(parseProgram(input)) // BLULZJLZ
  }
}
