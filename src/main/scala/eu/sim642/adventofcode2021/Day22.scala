package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.box.Box3
import eu.sim642.adventofcodelib.pos.Pos3

object Day22 {

  type Step = (Boolean, Box3)

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

  private val smallRegion = Box3(Pos3(-50, -50, -50), Pos3(50, 50, 50))

  def countRebootSmall(steps: Seq[Step]): Int = {
    val smallSteps = steps.filter(step => (step._2 union smallRegion) == smallRegion)
    simulateReboot(smallSteps).size
  }


  private val stepRegex = """(on|off) x=(-?\d+)\.\.(-?\d+),y=(-?\d+)\.\.(-?\d+),z=(-?\d+)\.\.(-?\d+)""".r

  def parseStep(s: String): Step = s match {
    case stepRegex(onOff, xMin, xMax, yMin, yMax, zMin, zMax) =>
      (onOff == "on", Box3(Pos3(xMin.toInt, yMin.toInt, zMin.toInt), Pos3(xMax.toInt, yMax.toInt, zMax.toInt)))
  }

  def parseSteps(input: String): Seq[Step] = input.linesIterator.map(parseStep).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countRebootSmall(parseSteps(input)))
  }
}
