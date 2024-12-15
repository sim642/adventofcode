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

  trait Part {
    def simulateMove(grid: Grid[Char], robot: Pos, move: Pos): (Grid[Char], Pos)
    def sumBoxGps(grid: Grid[Char]): Int

    def sumMovesBoxGps(input: Input): Int = {
      val initialRobot = input.grid.posOf('@')
      val initialGrid = input.grid.updatedGrid(initialRobot, '.')
      val (finalGrid, finalRobot) = input.moves.foldLeft((initialGrid, initialRobot))({ case ((grid, robot), move) =>
        simulateMove(grid, robot, move)
      })
      sumBoxGps(finalGrid)
    }
  }

  object Part1 extends Part {
    override def simulateMove(grid: Grid[Char], robot: Pos, move: Pos): (Grid[Char], Pos) = {
      val newRobot = robot + move
      val newBox = Iterator.iterate(newRobot)(_ + move).find(grid(_) != 'O').get
      if (grid(newBox) == '.')
        (grid.updatedGrid(newBox, 'O').updatedGrid(newRobot, '.'), newRobot)
      else
        (grid, robot)
    }

    override def sumBoxGps(grid: Grid[Char]): Int = {
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell == 'O'
      } yield 100 * y + x).sum
    }
  }

  object Part2 extends Part {
    def scaleGrid(grid: Grid[Char]): Grid[Char] = {
      grid.map(_.flatMap({
        case '#' => "##"
        case 'O' => "[]"
        case '.' => ".."
        case '@' => "@."
      }))
    }

    override def simulateMove(grid: Grid[Char], robot: Pos, move: Pos): (Grid[Char], Pos) = {

      def helper(grid: Grid[Char], pos: Pos): Option[Grid[Char]] = grid(pos) match {
        case '.' => Some(grid)
        case '#' => None
        case '[' if move.x == 0 => // vertical
          for {
            newGrid <- helper(grid, pos + move)
            newGrid2 <- helper(newGrid, pos + Pos(1, 0) + move)
          } yield newGrid2.updatedGrid(pos + move, '[').updatedGrid(pos + Pos(1, 0) + move, ']').updatedGrid(pos, '.').updatedGrid(pos + Pos(1, 0), '.')
        case ']' if move.x == 0 => // vertical
          for {
            newGrid <- helper(grid, pos + move)
            newGrid2 <- helper(newGrid, pos - Pos(1, 0) + move)
          } yield newGrid2.updatedGrid(pos + move, ']').updatedGrid(pos - Pos(1, 0) + move, '[').updatedGrid(pos, '.').updatedGrid(pos - Pos(1, 0), '.')
        case '[' if move.y == 0 => // horizontal
          for {
            newGrid <- helper(grid, pos + move)
          } yield newGrid.updatedGrid(pos + move, '[').updatedGrid(pos, '.')
        case ']' if move.y == 0 => // horizontal
          for {
            newGrid <- helper(grid, pos + move)
          } yield newGrid.updatedGrid(pos + move, ']').updatedGrid(pos, '.')
        case _ => ???
      }

      val newRobot = robot + move
      helper(grid, newRobot) match {
        case Some(newGrid) => (newGrid, newRobot)
        case None => (grid, robot)
      }
    }

    override def sumBoxGps(grid: Grid[Char]): Int = {
      printGrid(grid)
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell == '['
        //gpsX = x min (row.size - 1 - x)
        //gpsY = y min (grid.size - 1 - y)
        gpsX = x
        gpsY = y
        () = println((gpsX, gpsY))
      } yield 100 * gpsY + gpsX).sum
    }

    override def sumMovesBoxGps(input: Input): Int = {
      val scaledGrid = scaleGrid(input.grid)
      super.sumMovesBoxGps(Input(scaledGrid, input.moves))
    }
  }

  def parseGrid(s: String): Grid[Char] = s.linesIterator.map(_.toVector).toVector

  def parseMoves(s: String): Seq[Pos] = s.flatMap(moveOffsets.get) // ignores newlines

  def parseInput(input: String): Input = input match {
    case s"$grid\n\n$moves" => Input(parseGrid(grid), parseMoves(moves))
  }

  def printGrid(grid: Grid[Char]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell)
      println()
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.sumMovesBoxGps(parseInput(input)))
    println(Part2.sumMovesBoxGps(parseInput(input)))
  }
}
