package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day24 {

  // inspired by 2018 Day 18
  def step(grid: Grid[Boolean]): Grid[Boolean] = {
    // TODO: use view for zipWithIndex?
    for ((row, y) <- grid.zipWithIndex)
      yield for ((cell, x) <- row.zipWithIndex)
        yield {
          val pos = Pos(x, y)
          val neighbors = Pos.axisOffsets.map(pos + _).filter(grid.containsPos).map(grid(_))
          val bugs = neighbors.count(identity)
          cell match {
            case true if bugs != 1 => false
            case false if bugs == 1 || bugs == 2 => true
            case c => c
          }
        }
  }

  def findCycleGrid(grid: Grid[Boolean]): Grid[Boolean] = {
    NaiveCycleFinder.find(grid, step).cycleHead
  }

  def biodiversityRating(grid: Grid[Boolean]): Int = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell
      i = y * row.size + x
    } yield 1 << i).sum
  }

  def findCycleBiodiversityRating(grid: Grid[Boolean]): Int = biodiversityRating(findCycleGrid(grid))

  type RecGrid = Map[Int, Grid[Boolean]]
  type RecPos = (Int, Pos)

  def recStep(recGrid: RecGrid): RecGrid = {

    def recNeighbors(recPos: RecPos): Iterator[RecPos] = {
      val (level, pos) = recPos
      Pos.axisOffsets.iterator.flatMap({ offset =>
        val newPos = pos + offset
        newPos match {
          // outer neighbors
          case Pos(-1, _) => Iterator((level - 1, Pos(1, 2)))
          case Pos(5, _) => Iterator((level - 1, Pos(3, 2)))
          case Pos(_, -1) => Iterator((level - 1, Pos(2, 1)))
          case Pos(_, 5) => Iterator((level - 1, Pos(2, 3)))
          // inner neighbors
          case Pos(2, 2) =>
            offset match {
              case Pos(1, 0) => Iterator.tabulate(5)(y => (level + 1, Pos(0, y)))
              case Pos(-1, 0) => Iterator.tabulate(5)(y => (level + 1, Pos(4, y)))
              case Pos(0, 1) => Iterator.tabulate(5)(x => (level + 1, Pos(x, 0)))
              case Pos(0, -1) => Iterator.tabulate(5)(x => (level + 1, Pos(x, 4)))
              case _ => throw new IllegalArgumentException("illegal offset")
            }
          // level neighbors
          case pos => Iterator((level, pos))
        }
      })
    }

    val emptyGrid = Vector.fill(5, 5)(false)
    val levels = recGrid.keys
    // TODO: optimize by not creating useless empty levels
    val newRecGrid =
      (for ((level, grid) <- recGrid.iterator ++ Iterator((levels.min - 1, emptyGrid), (levels.max + 1, emptyGrid)))
        yield level -> (for ((row, y) <- grid.zipWithIndex)
          yield for ((cell, x) <- row.zipWithIndex)
            yield {
              val pos = Pos(x, y)
              if (pos == Pos(2, 2)) // midpoint is special and always false
                false
              else {
                val neighbors = recNeighbors((level, pos)).map({ case (level, pos) => recGrid.getOrElse(level, emptyGrid)(pos) })
                val bugs = neighbors.count(identity)
                cell match {
                  case true if bugs != 1 => false
                  case false if bugs == 1 || bugs == 2 => true
                  case c => c
                }
              }
            })).toMap

    newRecGrid
  }

  def countRecBugs(grid: Grid[Boolean], minutes: Int = 200): Int = {
    val recGrid = Map(0 -> grid)
    val finalRecGrid = Iterator.iterate(recGrid)(recStep)/*.tapEach(printRecGrid)*/(minutes)
    finalRecGrid.values.map(_.countGrid(identity)).sum
  }

  def printGrid(grid: Grid[Boolean]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(if (cell) '#' else '.')
      println()
    }
  }

  def printRecGrid(recGrid: RecGrid): Unit = {
    for ((level, grid) <- recGrid) {
      println(s"Level $level:")
      printGrid(grid)
    }
    println()
  }

  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.map(_ == '#').toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findCycleBiodiversityRating(parseGrid(input)))
    println(countRecBugs(parseGrid(input)))
  }
}
