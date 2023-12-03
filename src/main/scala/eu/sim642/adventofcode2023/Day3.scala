package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day3 {

  type Schematic = Vector[String]

  extension (schematic: Schematic) {
    def apply(pos: Pos): Char = schematic(pos.y)(pos.x)
  }

  def isSymbol(c: Char): Boolean = c != '.' && !c.isDigit

  def isGear(c: Char): Boolean = c == '*'

  private val numberRegex = """\d+""".r

  def iteratePartNumberBoxes(schematic: Schematic): Iterator[(Int, Box)] = {
    val schematicBox = Box(Pos.zero, Pos(schematic(0).length - 1, schematic.size - 1))

    for {
      (row, y) <- schematic.iterator.zipWithIndex
      m <- numberRegex.findAllMatchIn(row)
      box <- Box(Pos(m.start, y) - Pos(1, 1), Pos(m.end - 1, y) + Pos(1, 1)) intersect schematicBox
    } yield m.toString().toInt -> box
  }

  def sumPartNumbers(schematic: Schematic): Int = {
    iteratePartNumberBoxes(schematic)
      .filter(_._2.iterator.exists(p => isSymbol(schematic(p))))
      .map(_._1).sum
  }

  def sumGearRatios(schematic: Schematic): Int = {
    val gears = for {
      (number, box) <- iteratePartNumberBoxes(schematic)
      gearPos <- box.iterator
      if isGear(schematic(gearPos))
    } yield gearPos -> number

    gears.toSeq
      .groupMap(_._1)(_._2) // TODO: groupMap to IteratorImplicits
      .filter(_._2.size == 2)
      .map(_._2.product)
      .sum
  }


  def parseSchematic(input: String): Schematic = input.linesIterator.toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumPartNumbers(parseSchematic(input)))
    println(sumGearRatios(parseSchematic(input)))
  }
}
