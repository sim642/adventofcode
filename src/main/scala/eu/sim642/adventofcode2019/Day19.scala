package eu.sim642.adventofcode2019

import Intcode._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day19 {

  def isTractorBeam(program: Memory, pos: Pos): Boolean = {
    ProgramState(program, inputs = LazyList(pos.x, pos.y)).outputs.head != 0
  }

  def getGrid(program: Memory, size: Pos): Grid[Boolean] = {
    (for (y <- 0 until size.y)
      yield (for (x <- 0 until size.x)
        yield isTractorBeam(program, Pos(x, y))
      ).toVector
    ).toVector
  }

  def countTractorBeam(program: Memory, size: Pos = Pos(50, 50)): Int = getGrid(program, size).countGrid(identity)

  def findSquarePos(program: Memory, size: Pos): Pos = {
    val sizeOffset = Pos(-(size.x - 1), size.y - 1)

    @tailrec
    def helper(pos: Pos): Pos = {
      //assert(isTractorBeam(program, pos))

      if (isTractorBeam(program, pos + sizeOffset))
        pos + Pos(-(size.x - 1), 0) // rectangle found
      else if (isTractorBeam(program, pos + Pos(1, 0)))
        helper(pos + Pos(1, 0)) // right
      else
        helper(pos + Pos(0, 1)) // down
    }

    val start = Pos(16, 19) // TODO: don't hardcode contiguous start position
    helper(start)
  }

  def findSquareValue(program: Memory, size: Pos = Pos(100, 100)): Int = {
    val Pos(x, y) = findSquarePos(program, size)
    10000 * x + y
  }

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
    println(findSquareValue(parseProgram(input)))
  }
}
