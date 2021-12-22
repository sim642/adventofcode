package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.box.Box3
import eu.sim642.adventofcodelib.pos.Pos3

import scala.annotation.tailrec

object Day22 {

  // TODO: move to library?
  extension (box3: Box3) {
    def contains(otherBox3: Box3): Boolean = (otherBox3 union box3) == box3

    def size: BigInt = {
      val Box3(min, max) = box3
      BigInt(max.x - min.x + 1) * BigInt(max.y - min.y + 1) * BigInt(max.z - min.z + 1)
    }
  }

  type Step = (Boolean, Box3)

  private val initializationBox = Box3(Pos3(-50, -50, -50), Pos3(50, 50, 50))

  sealed trait Solution {
    def countReboot(steps: Seq[Step]): Long

    def countInitialization(steps: Seq[Step]): Int = {
      val smallSteps = steps.filter(step => initializationBox.contains(step._2))
      countReboot(smallSteps).toInt
    }
  }

  object NaiveSolution extends Solution {

    def simulateStep(poss: Set[Pos3], step: Step): Set[Pos3] = {
      val (on, box) = step
      if (on)
        poss ++ box.iterator
      else
        poss.filterNot(box.contains)
    }

    def simulateReboot(steps: Seq[Step]): Set[Pos3] = {
      steps.foldLeft(Set.empty[Pos3])(simulateStep)
    }

    override def countReboot(steps: Seq[(Boolean, Box3)]): Long = {
      simulateReboot(steps).size
    }
  }

  object InclusionExclusionSolution extends Solution {

    override def countReboot(steps: Seq[Step]): Long = {
      type Section = (Step, Int)

      @tailrec
      def helper(sections: Seq[Section], sign: Int, acc: BigInt): Long = {
        if (sections.isEmpty)
          acc.toLong
        else {
          val onSectionsSize =
            sections.view
              .collect({
                case ((true, box), _) => box.size
              })
              .sum
          val newAcc = acc + sign * onSectionsSize
          val newSections = for {
            ((sectionOn, sectionBox), sectionI) <- sections
            ((stepOn, stepBox), stepI) <- steps.view.zipWithIndex.drop(sectionI + 1) // only add steps to intersection in order
            if !(!sectionOn && stepOn) // no need to exclude active step from inactive section (intersection of all off)
            intersection <- sectionBox intersect stepBox
          } yield ((sectionOn || stepOn, intersection), stepI)
          helper(newSections, -sign, newAcc)
        }
      }

      helper(steps.zipWithIndex, +1, 0L)
    }
  }


  private val stepRegex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r

  def parseStep(s: String): Step = s match {
    case stepRegex(onOff, xMin, xMax, yMin, yMax, zMin, zMax) =>
      (onOff == "on", Box3(Pos3(xMin.toInt, yMin.toInt, zMin.toInt), Pos3(xMax.toInt, yMax.toInt, zMax.toInt)))
  }

  def parseSteps(input: String): Seq[Step] = input.linesIterator.map(parseStep).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import InclusionExclusionSolution._

    println(countInitialization(parseSteps(input)))
    println(countReboot(parseSteps(input)))
  }
}
