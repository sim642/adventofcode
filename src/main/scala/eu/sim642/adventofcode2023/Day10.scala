package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, Distances, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

import scala.annotation.tailrec

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

    val startAlternative = {
      val pos = grid.posOf('S')
      val newOffsets = for {
        offset <- pipeDirections(grid(pos))
        newPos = pos + offset
        if grid.containsPos(newPos)
        if grid(newPos) != '.'
        if pipeDirections(grid(newPos)).contains(-offset)
      } yield offset
      pipeDirections.find(_._2 == newOffsets).get._1
    }

    val loopGrid = {
      // TODO: use view for zipWithIndex?
      for ((row, y) <- grid.zipWithIndex)
        yield for ((cell, x) <- row.zipWithIndex)
          yield {
            val pos = Pos(x, y)
            if (cell == 'S')
              startAlternative
            else if (loop.contains(pos))
              cell
            else
              '.'
          }
    }

    def isInside(pos: Pos): Boolean = {

      @tailrec
      def helper(pos: Pos, count: Int): Boolean = {
        val newPos = pos + Pos(1, 0)
        if (grid.containsPos(newPos)) {
          loopGrid(newPos) match {
            case '|' | 'F' | '7' => helper(newPos, count + 1)
            case '.' | 'L' | 'J' => helper(newPos, count)
            case '-' => helper(newPos, count)
            case a => throw IllegalArgumentException(s"$a")
          }
        }
        else {
          count % 2 == 1
        }
      }

      helper(pos, 0)
    }

    (for {
      (row, y) <- loopGrid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '.'
      if isInside(Pos(x, y))
    } yield 1).sum
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(farthestDistance(parseGrid(input)))
    println(enclosedTiles(parseGrid(input)))
  }
}
