package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day3 {

  type Schematic = Vector[String]

  def isSymbol(c: Char): Boolean = c != '.' && !c.isDigit

  def isGear(c: Char): Boolean = c == '*'

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

  def sumGearRatios(schematic: Schematic): Int = {
    val schematicBox = Box(Pos.zero, Pos(schematic(0).length - 1, schematic.length - 1))

    val gears = for {
      (row, y) <- schematic.iterator.zipWithIndex
      m <- numberRegex.findAllMatchIn(row)
      box = Box(Pos(m.start, y) - Pos(1, 1), Pos(m.end - 1, y) + Pos(1, 1))
      gearPos <- box.iterator
      if schematicBox.contains(gearPos) && isGear(schematic(gearPos.y)(gearPos.x))
    } yield gearPos -> m.toString().toInt

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
