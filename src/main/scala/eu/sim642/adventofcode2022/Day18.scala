package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IteratorImplicits.IndexIteratorOps
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.pos.{Pos, Pos3}

import scala.annotation.tailrec
import scala.math.Integral.Implicits.*

object Day18 {

  def surfaceArea(droplets: Set[Pos3]): Int = {
    droplets
      .view
      .map(droplet =>
        Pos3.axisOffsets.view.map(droplet + _).count(!droplets.contains(_))
      )
      .sum
  }

  def parseDroplet(s: String): Pos3 = s match {
    case s"$x,$y,$z" => Pos3(x.toInt, y.toInt, z.toInt)
  }

  def parseDroplets(input: String): Set[Pos3] = input.linesIterator.map(parseDroplet).toSet

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(surfaceArea(parseDroplets(input)))
  }
}
