package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._

object Day8 {

  def findAntennas(grid: Grid[Char]): Map[Char, Set[Pos]] = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell != '.'
    } yield cell -> Pos(x, y)).toSet.groupMap(_._1)(_._2)
  }

  def antennaAntinodes(antenna1: Pos, antenna2: Pos): Set[Pos] = {
    val diff = antenna2 - antenna1
    Set(antenna1 - diff, antenna2 + diff)
  }

  def countAntinodes(grid: Grid[Char]): Int = {
    val antennas = findAntennas(grid)
    val antinodes = (for {
      (_, poss) <- antennas
      antenna1 <- poss
      antenna2 <- poss
      if antenna1 != antenna2
      antinode <- antennaAntinodes(antenna1, antenna2)
      if grid.containsPos(antinode)
    } yield antinode).toSet
    antinodes.size
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countAntinodes(parseGrid(input)))
  }
}
