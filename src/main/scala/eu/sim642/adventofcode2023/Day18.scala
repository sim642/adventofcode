package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Geometry
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.StringImplicits.*

object Day18 {

  case class DigStep(direction: Char, length: Int, color: String)

  type DigPlan = Seq[DigStep]

  private val moveOffsets = Map(
    'U' -> Pos(0, -1),
    'D' -> Pos(0, 1),
    'L' -> Pos(-1, 0),
    'R' -> Pos(1, 0),
  )

  trait Part {
    def extractSteps(digPlan: DigPlan): Seq[(Char, Int)]

    def lagoonSize(digPlan: DigPlan): Long = {
      val steps = extractSteps(digPlan)
      val vertices = steps.scanLeft(Pos.zero)({ case (pos, (direction, length)) =>
        pos + length *: moveOffsets(direction)
      })
      val area = Geometry.polygonArea[Long](vertices)
      val boundary = steps.map(_._2.toLong).sum
      val interior = area - boundary / 2 + 1 // Pick's theorem
      boundary + interior
    }
  }

  object Part1 extends Part {
    override def extractSteps(digPlan: DigPlan): Seq[(Char, Int)] =
      digPlan.map(step => (step.direction, step.length))
  }

  object Part2 extends Part {
    def parseColor(color: String): (Char, Int) = {
      val direction = color.last match {
        case '0' => 'R'
        case '1' => 'D'
        case '2' => 'L'
        case '3' => 'U'
      }
      (direction, color.take(5).toIntRadix(16))
    }

    override def extractSteps(digPlan: DigPlan): Seq[(Char, Int)] =
      digPlan.map(step => parseColor(step.color))
  }


  def parseDigStep(s: String): DigStep = s match {
    case s"$direction $length (#$color)" =>
      DigStep(direction.head, length.toInt, color)
  }

  def parseDigPlan(input: String): DigPlan = input.linesIterator.map(parseDigStep).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.lagoonSize(parseDigPlan(input)))
    println(Part2.lagoonSize(parseDigPlan(input)))
  }
}
