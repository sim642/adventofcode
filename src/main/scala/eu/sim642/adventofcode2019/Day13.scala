package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2017.Day22.OffsetPos
import eu.sim642.adventofcode2019.Day9._
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day13 {

  type Tile = Int

  def renderOutputs(outputs: LazyList[Value]): Map[Pos, Tile] = {
    outputs.grouped(3).foldLeft(Map.empty[Pos, Tile])({
      case (acc, LazyList(x, y, tile)) =>
        val pos = Pos(x.toInt, y.toInt)
        acc + (pos -> tile.toInt)
    })
  }

  def runPaint(program: Memory): Map[Pos, Tile] = {
    renderOutputs(ProgramState(program).outputs)
  }

  def countBlocks(program: Memory): Int = runPaint(program).values.count(_ == 2)

  def playGame(program: Memory): Value = {

    def renderOutputs(outputs: LazyList[Value]): (Map[Pos, Tile], Option[Value]) = {
      outputs.grouped(3).foldLeft((Map.empty[Pos, Tile], Option.empty[Value]))({
        case ((paint, _), LazyList(-1, 0, score)) =>
          (paint, Some(score))
        case ((paint, score), LazyList(x, y, tile)) =>
          val pos = Pos(x.toInt, y.toInt)
          (paint + (pos -> tile.toInt), score)
      })
    }

    @tailrec
    def helper(programState: ProgramState, prevPaint: Map[Pos, Tile], prevScore: Option[Value]): Value = {
      val execs = programState.execs
      if (execs.isEmpty)
        prevScore.get
      else {
        val lastState = execs.last._1
        val outputs = execs.flatMap(_._2)
        val (paint_, score) = renderOutputs(outputs)
        val paint = prevPaint ++ paint_
        //printPaint(paint)
        //println(score)

        val ballPos = paint.find(_._2 == 4).get._1 // linear search
        val paddlePos = paint.find(_._2 == 3).get._1 // linear search
        //println(ballPos, paddlePos)
        val newInput = {
          if (ballPos.x < paddlePos.x)
            -1
          else if (ballPos.x > paddlePos.x)
            1
          else
            0
        }

        helper(lastState.copy(inputs = LazyList(newInput)), paint, score)
      }
    }

    val newProgram = program + (0 -> 2L)
    helper(ProgramState(newProgram), Map.empty, None)
  }

  def printPaint(paint: Map[Pos, Tile]): Unit = {
    val Box(min, max) = Box.bounding(paint.keys)
    for (y <- min.y to max.y) {
      for (x <- min.x to max.x)
        print(paint.getOrElse(Pos(x, y), -1) match {
          case 0 => '.'
          case 1 => '#'
          case 2 => 'B'
          case 3 => '_'
          case 4 => 'O'

          case -1 => '?'
        })
      println()
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBlocks(parseProgram(input)))

    println(playGame(parseProgram(input)))
  }
}
