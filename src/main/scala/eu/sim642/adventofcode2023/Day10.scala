package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.*
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.{Geometry, Grid}

object Day10 {

  private val pipeDirections = Map(
    '|' -> Set(Pos(0, -1), Pos(0, 1)),
    '-' -> Set(Pos(1, 0), Pos(-1, 0)),
    'L' -> Set(Pos(0, -1), Pos(1, 0)),
    'J' -> Set(Pos(0, -1), Pos(-1, 0)),
    '7' -> Set(Pos(0, 1), Pos(-1, 0)),
    'F' -> Set(Pos(0, 1), Pos(1, 0)),
    'S' -> Pos.axisOffsets.toSet,
  )

  def loopTraversal(grid: Grid[Char]): GraphTraversal[Pos] with UnitNeighbors[Pos] = {
    new GraphTraversal[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = grid.posOf('S')

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- pipeDirections(grid(pos))
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) != '.'
          if pipeDirections(grid(newPos)).contains(-offset)
        } yield newPos
      }
    }
  }

  def farthestDistance(grid: Grid[Char]): Int =
    BFS.traverse(loopTraversal(grid)).distances.values.max

  trait Part2Solution {
    def enclosedTiles(grid: Grid[Char]): Int
  }

  /**
   * Solution, which determines inside using the ray casting algorithm.
   * @see [[https://en.wikipedia.org/wiki/Point_in_polygon#Ray_casting_algorithm]]
   */
  object RayCastingPart2Solution extends Part2Solution {
    override def enclosedTiles(grid: Grid[Char]): Int = {
      val loop = BFS.traverse(loopTraversal(grid)).nodes

      // grid with only loop: unconnected pipes removed and start pipe determined
      val loopGrid = {
        // TODO: use view for zipWithIndex?
        for ((row, y) <- grid.zipWithIndex)
          yield for ((cell, x) <- row.zipWithIndex)
            yield {
              val pos = Pos(x, y)
              if (cell == 'S') {
                val newOffsets = for {
                  offset <- pipeDirections(cell)
                  newPos = pos + offset
                  if grid.containsPos(newPos)
                  if grid(newPos) != '.'
                  if pipeDirections(grid(newPos)).contains(-offset)
                } yield offset
                pipeDirections.find(_._2 == newOffsets).get._1
              }
              else if (loop.contains(pos))
                cell
              else
                '.'
            }
      }

      // grid with pipe crossing count parity
      val insideGrid = {
        for (row <- loopGrid)
          yield row.scanLeft(false)({ // cast ray from left to right
            case (inside, '|' | 'L' | 'J') => !inside // consider pipe crossing at the top of tile
            case (inside, _) => inside
          })
      }

      (loopGrid lazyZip insideGrid)
        .map((loopRow, insideRow) =>
          (loopRow lazyZip insideRow)
            .count((cell, inside) => cell == '.' && inside)
        )
        .sum
    }
  }

  /**
   * Solution, which calculates inside points using Pick's theorem.
   * Area of the polygon is calculated using the shoelace formula.
   * @see [[https://en.wikipedia.org/wiki/Pick%27s_theorem]]
   */
  object PicksTheoremPart2Solution extends Part2Solution {
    override def enclosedTiles(grid: Grid[Char]): Int = {
      val loop = DFS.traverse(loopTraversal(grid)).nodeOrder
      Geometry.polygonArea(loop).toInt - loop.size / 2 + 1
    }
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import PicksTheoremPart2Solution.*

    println(farthestDistance(parseGrid(input)))
    println(enclosedTiles(parseGrid(input)))
  }
}
