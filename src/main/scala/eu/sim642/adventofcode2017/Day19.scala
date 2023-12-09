package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day19 {

  case class PathState(grid: Grid[Char], pos: Pos, prevOffset: Pos) {
    def char: Char = grid(pos)

    def hasNext: Boolean = char != ' '

    def next: PathState = char match {
      case ' ' =>
        this
      case '|' | '-' =>
        copy(pos = pos + prevOffset)
      case '+' =>
        val offsets = Pos.axisOffsets.toSet - (-prevOffset)
        val offset = offsets.find(offset => grid.containsPos(pos + offset) && grid(pos + offset) != ' ').get
        copy(pos = pos + offset, prevOffset = offset)
      case x if x.isLetter =>
        copy(pos = pos + prevOffset)
    }
  }

  def pathIterator(grid: Grid[Char]): Iterator[PathState] = {
    require(grid(0).contains('|'))

    val startX = grid(0).indexOf('|')
    Iterator.iterate(PathState(grid, Pos(startX, 0), Pos(0, 1)))(_.next).takeWhile(_.hasNext)
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def pathLetters(grid: Grid[Char]): String = pathIterator(grid).map(_.char).filter(_.isLetter).mkString

  def pathLetters(input: String): String = pathLetters(parseGrid(input))

  def pathLength(grid: Grid[Char]): Int = pathIterator(grid).size

  def pathLength(input: String): Int = pathLength(parseGrid(input))

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day19.txt")).mkString.stripLineEnd

  def main(args: Array[String]): Unit = {
    println(pathLetters(input))
    println(pathLength(input))
  }
}
