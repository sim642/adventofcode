package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day25 {

  case class Input(max: Pos, easts: Set[Pos], souths: Set[Pos])

  // TODO: deduplicate
  def stepEast(input: Input): Input = {
    val Input(max, easts, souths) = input
    val newEasts = for {
      pos@Pos(x, y) <- easts
      newPos = Pos((x + 1) % max.x, y)
      newPos2 =
        if (!easts.contains(newPos) && !souths.contains(newPos))
          newPos
        else
          pos
    } yield newPos2
    input.copy(easts = newEasts)
  }

  def stepSouth(input: Input): Input = {
    val Input(max, easts, souths) = input
    val newSouths = for {
      pos@Pos(x, y) <- souths
      newPos = Pos(x, (y + 1) % max.y)
      newPos2 =
        if (!easts.contains(newPos) && !souths.contains(newPos))
          newPos
        else
          pos
    } yield newPos2
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

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findStoppedStep(parseInput(input))) // 305
  }
}
