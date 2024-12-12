package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph.{BFS, GraphComponents}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcode2018.Day13.DirectionPos

object Day12 {

  case class Edge(in: Pos, out: Pos)

  case class Region(poss: collection.Set[Pos]) {
    def area: Int = poss.size

    def perimeter: Int = {
      poss.iterator // iterator to avoid deduplicating same edge counts
        .map(pos => 4 - Pos.axisOffsets.map(pos + _).count(poss))
        .sum
    }

    def sides: Int = {
      val edges =
        for {
          pos <- poss
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if !poss(newPos)
        } yield Edge(pos, newPos)

      val graphComponents = new GraphComponents[Edge] {
        override def nodes: IterableOnce[Edge] = edges

        override def unitNeighbors(edge: Edge): IterableOnce[Edge] = {
          val Edge(in, out) = edge
          val direction = out - in
          for {
            inOffset <- Seq(direction.left, direction.right)
            newIn = in + inOffset
            newOut = newIn + direction
            newEdge = Edge(newIn, newOut)
            if edges(newEdge)
          } yield newEdge
        }
      }

      BFS.components(graphComponents).size
    }
  }

  def regions(grid: Grid[Char]): collection.Set[Region] = {
    val graphComponents = new GraphComponents[Pos] {
      override def nodes: IterableOnce[Pos] = Box(Pos.zero, Pos(grid(0).size - 1, grid.size - 1)).iterator

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        val region = grid(pos)
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) == region
        } yield newPos
      }
    }

    BFS.components(graphComponents).map(Region.apply)
  }

  trait Part {
    def regionFencingPrice(region: Region): Int

    def totalFencingPrice(grid: Grid[Char]): Int =
      regions(grid).iterator.map(regionFencingPrice).sum
  }

  object Part1 extends Part {
    override def regionFencingPrice(region: Region): Int = region.area * region.perimeter
  }

  object Part2 extends Part {
    override def regionFencingPrice(region: Region): Int = region.area * region.sides
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.totalFencingPrice(parseGrid(input)))
    println(Part2.totalFencingPrice(parseGrid(input)))
  }
}
