package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day25 {

  extension (pos: Pos) {
    def %(other: Pos): Pos = Pos(pos.x % other.x, pos.y % other.y)
  }

  case class Input(max: Pos, easts: Set[Pos], souths: Set[Pos]) {
    def contains(pos: Pos): Boolean = easts.contains(pos) || souths.contains(pos)
  }

  def stepOffset(input: Input, poss: Set[Pos], offset: Pos): Set[Pos] = {
    val Input(max, easts, souths) = input
    for {
      pos <- poss
      newPos = (pos + offset) % max
      newPos2 =
        if (!input.contains(newPos))
          newPos
        else
          pos
    } yield newPos2
  }

  def stepEast(input: Input): Input = {
    val newEasts = stepOffset(input, input.easts, Pos(1, 0))
    input.copy(easts = newEasts)
  }

  def stepSouth(input: Input): Input = {
    val newSouths = stepOffset(input, input.souths, Pos(0, 1))
    input.copy(souths = newSouths)
  }

  def step(input: Input): Input = stepSouth(stepEast(input))

  def findStoppedStep(input: Input): Int = {
    Iterator.iterate(input)(step).zipWithTail.indexWhere(p => p._1 == p._2) + 1
  }


  def parseInput(input: String): Input = {
    val grid = input.linesIterator.map(_.toVector).toVector
    val max = Pos(grid(0).size, grid.size)
    val easts = (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '>'
    } yield Pos(x, y)).toSet
    val souths = (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == 'v'
    } yield Pos(x, y)).toSet
    Input(max, easts, souths)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findStoppedStep(parseInput(input))) // 305
  }
}
