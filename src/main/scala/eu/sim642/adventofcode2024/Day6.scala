package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

object Day6 {

  case class Guard(pos: Pos, direction: Pos)

  case class Input(grid: Grid[Boolean], guard: Guard)

  def stepGuard(grid: Grid[Boolean])(guard: Guard): Guard = {
    val newPos = guard.pos + guard.direction
    if (grid.containsPos(newPos) && grid(newPos))
      guard.copy(direction = guard.direction.right)
    else
      guard.copy(pos = newPos)
  }

  def iterateGuard(input: Input): Iterator[Guard] =
    Iterator.iterate(input.guard)(stepGuard(input.grid))

  def countGuardPoss(input: Input): Int = {
    iterateGuard(input)
      .takeWhile(guard => input.grid.containsPos(guard.pos))
      .map(_.pos)
      .toSet
      .size
  }

  def isGuardCycle(input: Input): Boolean = {
    val it = iterateGuard(input).takeWhile(guard => input.grid.containsPos(guard.pos))
    val cycle = NaiveCycleFinder.find(it)
    cycle.isDefined
  }

  def countObstructionPoss(input: Input): Int = { // TODO: optimize
    val Input(grid, guard) = input
    Box(Pos.zero, Pos(grid(8).size - 1, grid.size - 1))
      .iterator
      .filter(obstructionPos => obstructionPos != guard.pos && !grid(obstructionPos))
      .map(obstructionPos => Input(grid.updatedGrid(obstructionPos, true), guard))
      .count(isGuardCycle)
  }

  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid({
    case '#' => true
    case '.' => false
  })

  def parseInput(input: String): Input = {
    val rawGrid = input.linesIterator.map(_.toVector).toVector
    val guard = Guard(rawGrid.posOf('^'), Pos(0, -1))
    val grid = rawGrid.mapGrid({
      case '#' => true
      case _ => false
    })
    Input(grid, guard)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countGuardPoss(parseInput(input)))
    println(countObstructionPoss(parseInput(input)))
  }
}
