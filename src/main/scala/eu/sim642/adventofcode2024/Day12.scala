package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph.{BFS, GraphComponents}
import eu.sim642.adventofcodelib.pos.Pos

object Day12 {

  case class Region(poss: Set[Pos]) {
    def area: Int = poss.size

    def perimeter: Int = {
      4 * area -
      poss.iterator
        .map(pos => Pos.axisOffsets.map(pos + _).count(poss)) //.tapEach(println)
        .sum
    }
  }

  def regions(grid: Grid[Char]): Set[Region] = {
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

    BFS.components(graphComponents).map(x => Region(x.toSet)).toSet // TODO: avoid weird toSet-s
  }

  def totalFencingPrice(grid: Grid[Char]): Int = {
    val regs = regions(grid)
    //for (region <- regs)
    //  println(s"${grid(region.poss.head)}: area=${region.area} perimeter=${region.perimeter}")
    regs.iterator.map(region => region.area * region.perimeter).sum
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalFencingPrice(parseGrid(input)))
  }
}
