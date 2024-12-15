package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._

object Day15 {

  private val moveOffsets = Map(
    '^' -> Pos(0, -1),
    '>' -> Pos(1, 0),
    'v' -> Pos(0, 1),
    '<' -> Pos(-1, 0),
  )

  case class Input(grid: Grid[Char], moves: Seq[Pos])

  def simulateMove(grid: Grid[Char], robot: Pos, move: Pos): (Grid[Char], Pos) = {
    val newRobot = robot + move
    val newBox = Iterator.iterate(newRobot)(_ + move).find(grid(_) != 'O').get
    if (grid(newBox) == '.')
      (grid.updatedGrid(newBox, 'O').updatedGrid(newRobot, '.'), newRobot)
    else
      (grid, robot)
  }

  def sumBoxGps(grid: Grid[Char]): Int = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == 'O'
    } yield 100 * y + x).sum
  }

  def sumMovesBoxGps(input: Input): Int = {
    val initialRobot = input.grid.posOf('@')
    val initialGrid = input.grid.updatedGrid(initialRobot, '.')
    val (finalGrid, finalRobot) = input.moves.foldLeft((initialGrid, initialRobot))({ case ((grid, robot), move) =>
      simulateMove(grid, robot, move)
    })
    sumBoxGps(finalGrid)
  }

  def parseGrid(s: String): Grid[Char] = s.linesIterator.map(_.toVector).toVector

  def parseMoves(s: String): Seq[Pos] = s.flatMap(moveOffsets.get) // ignores newlines

  def parseInput(input: String): Input = input match {
    case s"$grid\n\n$moves" => Input(parseGrid(grid), parseMoves(moves))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumMovesBoxGps(parseInput(input)))
  }
}
