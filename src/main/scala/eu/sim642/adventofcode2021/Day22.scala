package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.box.Box3
import eu.sim642.adventofcodelib.pos.Pos3

object Day22 {

  type Step = (Boolean, Box3)

  private val smallRegion = Box3(Pos3(-50, -50, -50), Pos3(50, 50, 50))

  sealed trait Solution {
    def countReboot(steps: Seq[Step]): Long

    def countRebootSmall(steps: Seq[Step]): Int = {
      val smallSteps = steps.filter(step => (step._2 union smallRegion) == smallRegion)
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
      /*val pairoverlaps = steps.combinations(2).count({
      case Seq((_, box1), (_, box2)) =>
        (box1 intersect box2).isDefined
    })
    val tripleoverlaps = steps.combinations(3).count({
      case Seq((_, box1), (_, box2), (_, box3)) =>
        (box1 intersect box2).flatMap(_ intersect box3).isDefined
    })
    val quadoverlaps = steps.combinations(4).count({
      case Seq((_, box1), (_, box2), (_, box3), (_, box4)) =>
        (box1 intersect box2).flatMap(_ intersect box3).flatMap(_ intersect box4).isDefined
    })
    println(steps.size)
    println(pairoverlaps)
    println(steps.size * (steps.size - 1) / 2)
    println(tripleoverlaps)
    println(quadoverlaps)*/

      type Section = (Step, Set[Int])

      def box3Size(box: Box3): BigInt = {
        val Box3(min, max) = box
        val r = BigInt(max.x - min.x + 1) * BigInt(max.y - min.y + 1) * BigInt(max.z - min.z + 1)
        r
      }

      def helper(sections: Set[Section], sign: Int, acc: BigInt): Long = {
        //println(sections)
        val thing = sign * sections.view.map({ case ((on, box), _) =>
          val r = if (on) box3Size(box) else BigInt(0)
          //println(r)
          r
        }).sum
        val newAcc = acc + thing
        //println(s"$acc $thing")
        if (sections.isEmpty) {
          //println("----------------------")
          newAcc.toLong
        }
        else {
          //println(sections.size)
          val newSections = for {
            ((sectionOn, sectionBox), combinedSteps) <- sections
            ((stepOn, stepBox), i) <- steps.zipWithIndex
            //if !combinedSteps.contains(i)
            if i > combinedSteps.max
            if !(!sectionOn && stepOn)
            interBox <- sectionBox intersect stepBox
            //} yield ((if (i > combinedSteps.max) stepOn else sectionOn, interBox), combinedSteps + i)
          } yield ((stepOn || sectionOn, interBox), combinedSteps + i)
          helper(newSections, -sign, newAcc)
        }
      }

      helper(steps.zipWithIndex.map((step, i) => (step, Set(i))).toSet, +1, 0L)
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

    println(countRebootSmall(parseSteps(input)))
    println(countReboot(parseSteps(input)))
  }
}
