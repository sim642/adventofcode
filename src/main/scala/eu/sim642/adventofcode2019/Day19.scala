package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.pos.Pos

object Day19 {

  def getGrid(program: Memory, size: Pos): Grid[Boolean] = {
    (for (y <- 0 until size.y)
      yield (for (x <- 0 until size.x)
        yield {
          val output = ProgramState(program, inputs = LazyList(x, y)).outputs.head != 0
          output
        }
      ).toVector
    ).toVector
  }

  def countTractorBeam(program: Memory, size: Pos = Pos(50, 50)): Int = getGrid(program, size).countGrid(identity)

  def printGrid(grid: Grid[Boolean]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(if (cell) '#' else '.')
      println()
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    printGrid(getGrid(parseProgram(input), Pos(50, 50)))

    println(countTractorBeam(parseProgram(input)))
  }
}
