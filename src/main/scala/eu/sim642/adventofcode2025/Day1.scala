package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.IntegralImplicits._

object Day1 {

  def actualPassword(rotations: Seq[Int]): Int = {
    rotations
      .scanLeft[Int](50)((a, b) => (a + b) %+ 100) // TODO: why can't use implicit arguments?
      .count(_ == 0)
  }

  def parseRotation(s: String): Int = s match {
    case s"L$i" => -i.toInt
    case s"R$i" => i.toInt
  }

  def parseRotations(input: String): Seq[Int] = input.linesIterator.map(parseRotation).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(actualPassword(parseRotations(input)))
  }
}
