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
    val maxCheat: Int

    def findCheats(grid: Grid[Char]): Set[Cheat] = {
      val forwardSearch = gridGraphSearch(grid, 'S', 'E')
      val forwardResult = BFS.search(forwardSearch)
      val backwardSearch = gridGraphSearch(grid, 'E', 'S')
      val backwardResult = BFS.search(backwardSearch)

      val noCheatDistance = forwardResult.target.get._2

      // TODO: optimize

      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell != '#'
        start = Pos(x, y)
        xOffset <- -maxCheat to maxCheat
        pos = start + Pos(xOffset, 0)
        if grid.containsPos(pos)
        startCheat = xOffset.abs
        maxEndCheat = maxCheat - startCheat
        yOffset <- (-maxEndCheat) to maxEndCheat
        end = pos + Pos(0, yOffset)
        if grid.containsPos(end) && grid(end) != '#'
        endCheat = yOffset.abs
        cheatDistance = forwardResult.distances(start) + (startCheat + endCheat) + backwardResult.distances(end)
        //if cheatDistance <= noCheatDistance
        save = noCheatDistance - cheatDistance
      } yield Cheat(start, end, save)).toSet
    }

    def countGoodCheats(grid: Grid[Char]): Int = findCheats(grid).count(_.save >= 100)
  }

  object Part1 extends Part {
    override val maxCheat: Int = 2
  }

  object Part2 extends Part {
    override val maxCheat: Int = 20
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countGoodCheats(parseGrid(input)))
    println(Part2.countGoodCheats(parseGrid(input)))
  }
}
