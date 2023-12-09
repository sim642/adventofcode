package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.box.Box3
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos3

object Day18 {

  def surfaceArea(droplets: Set[Pos3]): Int = {
    droplets
      .view
      .map(droplet =>
        Pos3.axisOffsets
          .view
          .map(droplet + _)
          .count(!droplets.contains(_))
      )
      .sum
  }

  def exteriorSurfaceArea(droplets: Set[Pos3]): Int = {
    val box = Box3.bounding(droplets)
    val paddedBox = Box3(box.min + Pos3(-1, -1, -1), box.max + Pos3(1, 1, 1)) // pad bounding box to allow traversal around

    val graphTraversal = new GraphTraversal[Pos3] with UnitNeighbors[Pos3] {
      override val startNode: Pos3 = paddedBox.min

      override def unitNeighbors(pos: Pos3): IterableOnce[Pos3] = {
        for {
          offset <- Pos3.axisOffsets
          newPos = pos + offset
          if paddedBox.contains(newPos) && !droplets.contains(newPos)
        } yield newPos
      }
    }

    val exterior = BFS.traverse(graphTraversal).nodes

    droplets
      .view
      .map(droplet =>
        Pos3.axisOffsets
          .view
          .map(droplet + _)
          .count(exterior.contains)
      )
      .sum
  }

  def parseDroplet(s: String): Pos3 = s match {
    case s"$x,$y,$z" => Pos3(x.toInt, y.toInt, z.toInt)
  }

  def parseDroplets(input: String): Set[Pos3] = input.linesIterator.map(parseDroplet).toSet

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(surfaceArea(parseDroplets(input)))
    println(exteriorSurfaceArea(parseDroplets(input)))
  }
}
