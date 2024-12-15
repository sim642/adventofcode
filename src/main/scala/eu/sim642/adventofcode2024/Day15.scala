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
    val boxGpsChar: Char

    def sumBoxGps(grid: Grid[Char]): Int = {
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell == boxGpsChar
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

    override val boxGpsChar: Char = 'O'
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

      def helper(grid: Grid[Char], pos: Pos): Option[Grid[Char]] = {
        grid(pos) match {
          case '.' => Some(grid)
          case '#' => None
          case '[' if move.x == 0 => // vertical
            val newPos = pos + move
            for {
              newGrid <- helper(grid, newPos)
              newGrid2 <- helper(newGrid, newPos + Pos(1, 0))
            } yield
              newGrid2
                .updatedGrid(newPos, '[')
                .updatedGrid(newPos + Pos(1, 0), ']')
                .updatedGrid(pos, '.')
                .updatedGrid(pos + Pos(1, 0), '.')
          case ']' if move.x == 0 => // vertical
            helper(grid, pos - Pos(1, 0)) // handle [ instead
          case cell@('[' | ']') if move.y == 0 => // horizontal
            val newPos = pos + move
            for {
              newGrid <- helper(grid, newPos)
            } yield
              newGrid
                .updatedGrid(newPos, cell)
                .updatedGrid(pos, '.')
          case _ => throw new IllegalArgumentException("illegal grid cell")
        }
      }

      val newRobot = robot + move
      helper(grid, newRobot) match {
        case Some(newGrid) => (newGrid, newRobot)
        case None => (grid, robot)
      }
    }

    // The text is being unnecessarily confusing/misleading here, referring to "closest",
    // when it still means left and top...
    override val boxGpsChar: Char = '['

    override def sumMovesBoxGps(input: Input): Int = {
      val scaledGrid = scaleGrid(input.grid)
      super.sumMovesBoxGps(input.copy(grid = scaledGrid))
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
