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

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBlocks(parseProgram(input)))
  }
}
