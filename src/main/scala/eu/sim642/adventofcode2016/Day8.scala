package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2017.Day19.Grid
import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2017.Day14.PosGrid
import eu.sim642.adventofcode2018.Day6.iterateRect

object Day8 {

  sealed trait Operation
  case class Rect(a: Int, b: Int) extends Operation
  case class RotateRow(y: Int, by: Int) extends Operation
  case class RotateColumn(x: Int, by: Int) extends Operation

  def litPixels(operations: Seq[Operation]): Int = {
    // assumes no pixel is overdrawn
    operations.map({
      case Rect(a, b) => a * b
      case _ => 0
    }).sum
  }

  implicit class PosUpdatedGrid[A](grid: Grid[A]) {
    def updatedGrid(pos: Pos, elem: A): Grid[A] = {
      grid.updated(pos.y, grid(pos.y).updated(pos.x, elem))
    }
  }

  def execute(grid: Grid[Boolean], operation: Operation): Grid[Boolean] = operation match {
    case Rect(a, b) =>
      iterateRect(Pos(0, 0), Pos(a - 1, b - 1)).foldLeft(grid)(_.updatedGrid(_, true))
    case RotateRow(y, by) =>
      grid(y).indices.map(Pos(_, y)).foldLeft(grid)({ (acc, pos) =>
        val newPos = Pos((pos.x + by) % grid(y).size, pos.y)
        acc.updatedGrid(newPos, grid(pos))
      })
    case RotateColumn(x, by) =>
      grid.indices.map(Pos(x, _)).foldLeft(grid)({ (acc, pos) =>
        val newPos = Pos(pos.x, (pos.y + by) % grid.size)
        acc.updatedGrid(newPos, grid(pos))
      })
  }

  def execute(grid: Grid[Boolean], operations: Seq[Operation]): Grid[Boolean] = {
    operations.foldLeft(grid)(execute)
  }

  def printGrid(grid: Grid[Boolean]): Unit = {
    for (row <- grid) {
      for (cell <- row) {
        print(if (cell) '#' else '.')
      }
      println()
    }
  }

  def render(operations: Seq[Operation]): Unit = {
    val initialGrid = Vector.fill(6)(Vector.fill(50)(false))
    val finalGrid = execute(initialGrid, operations)
    printGrid(finalGrid)
  }

  private val rectRegex = """rect (\d+)x(\d+)""".r
  private val rotateRowRegex = """rotate row y=(\d+) by (\d+)""".r
  private val rotateColumnRegex = """rotate column x=(\d+) by (\d+)""".r

  def parseOperation(s: String): Operation = s match {
    case rectRegex(a, b) => Rect(a.toInt, b.toInt)
    case rotateRowRegex(y, by) => RotateRow(y.toInt, by.toInt)
    case rotateColumnRegex(x, by) => RotateColumn(x.toInt, by.toInt)
  }

  def parseOperations(input: String): Seq[Operation] = input.lines.map(parseOperation).toSeq

  def litPixels(input: String): Int = litPixels(parseOperations(input))

  def render(input: String): Unit = render(parseOperations(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(litPixels(input))
    render(input) // CFLELOYFCS
  }
}
