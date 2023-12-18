package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Geometry
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.StringImplicits.*
import eu.sim642.adventofcodelib.IteratorImplicits.*

object Day18 {

  case class DigStep(direction: Char, length: Int, color: String)

  type DigPlan = Seq[DigStep]

  private val moveOffsets = Map(
    'U' -> Pos(0, -1),
    'D' -> Pos(0, 1),
    'L' -> Pos(-1, 0),
    'R' -> Pos(1, 0),
  )

  def digTrench(digPlan: DigPlan): Set[Pos] = {
    digPlan.iterator
      .flatMap(step => Iterator.fill(step.length)(step.direction))
      .scanLeft(Pos.zero)((pos, direction) => pos + moveOffsets(direction))
      .toSet
  }

  def digInterior(digPlan: DigPlan): Set[Pos] = {
    val trench = digTrench(digPlan)

    val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = Pos(1, 1)

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if !trench(newPos)
        } yield newPos
      }
    }

    val interior = BFS.traverse(graphTraversal).nodes
    trench ++ interior
  }

  def lagoonSize(digPlan: DigPlan): Int = digInterior(digPlan).size

  def parseColor(color: String): (Char, Int) = {
    val direction = color.last match {
      case '0' => 'R'
      case '1' => 'D'
      case '2' => 'L'
      case '3' => 'U'
    }
    (direction, color.take(5).toIntRadix(16))
  }

  def lagoonSize2(digPlan: DigPlan): Long = {
    val plan2 = digPlan
      .map(step => parseColor(step.color))
    val points = plan2
      .scanLeft(Pos.zero)({ case (pos, (direction, length)) => pos + length *: moveOffsets(direction) })
    //Geometry.polygonArea(points)
    val area = (points.iterator
      .zipWithTail
      .map((p, q) => p.x.toLong * q.y - q.x.toLong * p.y)
      .sum / 2).abs
    val perim = plan2.map(_._2.toLong).sum
    area + perim / 2 + 1
  }


  def parseDigStep(s: String): DigStep = s match {
    case s"$direction $length (#$color)" =>
      DigStep(direction.head, length.toInt, color)
  }

  def parseDigPlan(input: String): DigPlan = input.linesIterator.map(parseDigStep).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(lagoonSize(parseDigPlan(input)))
    println(lagoonSize2(parseDigPlan(input)))
  }
}
