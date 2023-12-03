package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day3 {

  type Schematic = Vector[String]

  def isSymbol(c: Char): Boolean = c != '.' && !c.isDigit

  private val numberRegex = """\d+""".r

  def iteratePartNumbers(schematic: Schematic): Iterator[Int] = {
    val schematicBox = Box(Pos.zero, Pos(schematic(0).length - 1, schematic.length - 1))

    for {
      (row, y) <- schematic.iterator.zipWithIndex
      m <- numberRegex.findAllMatchIn(row)
      box = Box(Pos(m.start, y) - Pos(1, 1), Pos(m.end - 1, y) + Pos(1, 1))
      if box.iterator.exists(p => schematicBox.contains(p) && isSymbol(schematic(p.y)(p.x)))
    } yield m.toString().toInt
  }

  def sumPartNumbers(schematic: Schematic): Int = iteratePartNumbers(schematic).sum


  def parseSchematic(input: String): Schematic = input.linesIterator.toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPartNumbers(parseSchematic(input)))
  }
}
