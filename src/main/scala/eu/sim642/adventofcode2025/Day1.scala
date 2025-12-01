package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.IntegralImplicits._

object Day1 {

  trait Part {
    def password(rotations: Seq[Int]): Int
  }

  object Part1 extends Part {
    override def password(rotations: Seq[Int]): Int = {
      rotations
        .scanLeft[Int](50)((a, b) => (a + b) %+ 100) // TODO: why can't use implicit arguments?
        .count(_ == 0)
    }
  }

  object Part2 extends Part {
    override def password(rotations: Seq[Int]): Int = {
      rotations
        .flatMap({ // expand all rotations to single to make each tick observable, this is silly but works
          case i if i >= 0 => Seq.fill(i)(1)
          case i if i < 0 => Seq.fill(-i)(-1)
        })
        .scanLeft[Int](50)((a, b) => (a + b) %+ 100) // TODO: why can't use implicit arguments?
        .count(_ == 0)
    }
  }

  def parseRotation(s: String): Int = s match {
    case s"L$i" => -i.toInt
    case s"R$i" => i.toInt
  }

  def parseRotations(input: String): Seq[Int] = input.linesIterator.map(parseRotation).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.password(parseRotations(input)))
    println(Part2.password(parseRotations(input)))
  }
}
