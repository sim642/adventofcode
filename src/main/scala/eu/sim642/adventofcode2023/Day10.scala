package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, Distances, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

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

  def findLoop(grid: Grid[Char]): Distances[Pos] = {
    val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
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

    BFS.traverse(graphTraversal)
  }

  def farthestDistance(grid: Grid[Char]): Int = findLoop(grid).distances.values.max

  def enclosedTiles(grid: Grid[Char]): Int = {
    val loop = findLoop(grid).nodes

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


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(farthestDistance(parseGrid(input)))
    println(enclosedTiles(parseGrid(input)))
  }
}
