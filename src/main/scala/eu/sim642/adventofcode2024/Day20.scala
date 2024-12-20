package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

object Day20 {

  case class Cheat(start: Pos, end: Pos, save: Int)

  def gridGraphSearch(grid: Grid[Char], start: Char, end: Char): GraphSearch[Pos] & UnitNeighbors[Pos] & TargetNode[Pos] = {
    new GraphSearch[Pos] with UnitNeighbors[Pos] with TargetNode[Pos] {
      override val startNode: Pos = grid.posOf(start)

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid(newPos) != '#'
        } yield newPos
      }

      override val targetNode: Pos = grid.posOf(end)
    }
  }

  trait Part {
    def findCheats(grid: Grid[Char]): Set[Cheat]

    def countGoodCheats(grid: Grid[Char]): Int = findCheats(grid).count(_.save >= 100)
  }

  object Part1 extends Part {
    override def findCheats(grid: Grid[Char]): Set[Cheat] = {
      val forwardSearch = gridGraphSearch(grid, 'S', 'E')
      val forwardResult = BFS.search(forwardSearch)
      val backwardSearch = gridGraphSearch(grid, 'E', 'S')
      val backwardResult = BFS.search(backwardSearch)

      val noCheatDistance = forwardResult.target.get._2

      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell == '#'
        pos = Pos(x, y)
        startOffset <- Pos.axisOffsets
        start = pos + startOffset
        if grid.containsPos(start) && grid(start) != '#'
        endOffset <- Pos.axisOffsets
        if startOffset != endOffset
        end = pos + endOffset
        if grid.containsPos(end) && grid(end) != '#'
        cheatDistance = forwardResult.distances(start) + 2 + backwardResult.distances(end)
        //if cheatDistance <= noCheatDistance
        save = noCheatDistance - cheatDistance
      } yield Cheat(start, end, save)).toSet
    }
  }

  object Part2 extends Part {
    override def findCheats(grid: Grid[Char]): Set[Cheat] = {
      val forwardSearch = gridGraphSearch(grid, 'S', 'E')
      val forwardResult = BFS.search(forwardSearch)
      val backwardSearch = gridGraphSearch(grid, 'E', 'S')
      val backwardResult = BFS.search(backwardSearch)

      val noCheatDistance = forwardResult.target.get._2

      // TODO: optimize

      /*(for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell == '.'
        start = Pos(x, y)
        startOffset <- Pos.axisOffsets
        startCheat <- 0 to 20
        pos = start + startCheat *: startOffset
        if grid.containsPos(pos)
        endOffset <- Pos.axisOffsets
        if startOffset != endOffset && startOffset != -endOffset
        endCheat <- 0 to (20 - startCheat)
        end = pos + endCheat *: endOffset
        if grid.containsPos(end) && grid(end) != '#'
        cheatDistance = forwardResult.distances(start) + (startCheat + endCheat) + backwardResult.distances(end)
        //if cheatDistance <= noCheatDistance
        save = noCheatDistance - cheatDistance
      } yield Cheat(start, end, save)).toSet*/

      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell != '#'
        start = Pos(x, y)
        xOffset <- -20 to 20
        pos = start + Pos(xOffset, 0)
        if grid.containsPos(pos)
        startCheat = xOffset.abs
        maxEndCheat = 20 - startCheat
        yOffset <- (-maxEndCheat) to maxEndCheat
        end = pos + Pos(0, yOffset)
        if grid.containsPos(end) && grid(end) != '#'
        endCheat = yOffset.abs
        cheatDistance = forwardResult.distances(start) + (startCheat + endCheat) + backwardResult.distances(end)
        //if cheatDistance <= noCheatDistance
        save = noCheatDistance - cheatDistance
      } yield Cheat(start, end, save)).toSet
    }
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countGoodCheats(parseGrid(input)))
    println(Part2.countGoodCheats(parseGrid(input)))
  }
}
