package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2017.Day22.OffsetPos
import intcode._
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
      case (_, _) =>
        throw new IllegalArgumentException("incomplete output")
    })
  }

  def runPaint(program: Memory): Map[Pos, Tile] = {
    renderOutputs(ProgramState(program).outputs)
  }

  def countBlocks(program: Memory): Int = runPaint(program).values.count(_ == 2)


  case class PlayState(paddlePos: Option[Pos],
                       ballPos: Option[Pos],
                       score: Option[Value]) {

    def updated(pos: Pos, value: Value): PlayState = (pos, value) match {
      case (Pos(-1, 0), score) => // score
        copy(score = Some(score))
      case (pos, 3) => // paddle
        copy(paddlePos = Some(pos))
      case (pos, 4) => // ball
        copy(ballPos = Some(pos))
      case (_, _) =>
        this
    }
  }

  def playGame(program: Memory): Value = {

    @tailrec
    def helper(programState: ProgramState, playState: PlayState): Value = {
      val execs = programState.execs
      if (execs.isEmpty) // halted
        playState.score.get
      else {
        val outputs = execs.flatMap(_._2)
        val newPlayState = outputs.grouped(3).foldLeft(playState)({
          case (prevState, LazyList(x, y, value)) =>
            prevState.updated(Pos(x.toInt, y.toInt), value)
          case (_, _) =>
            throw new IllegalArgumentException("incomplete output")
        })

        val ballPosX = newPlayState.ballPos.get.x
        val paddlePosX = newPlayState.paddlePos.get.x
        val newInput = (ballPosX `compareTo` paddlePosX).sign // use .sign to clamp to -1, 0, 1

        val newProgramState = execs.last._1.copy(inputs = LazyList(newInput))
        helper(newProgramState, newPlayState)
      }
    }

    val newProgram = program + (0 -> 2L)
    helper(ProgramState(newProgram), PlayState(None, None, None))
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBlocks(parseProgram(input)))
    println(playGame(parseProgram(input)))
  }
}
