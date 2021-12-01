package eu.sim642.adventofcode2021

object Day1 {

  def countIncreases(depths: Seq[Int]): Int = {
    depths.sliding(2).count({
      case Seq(a, b) if a < b => true
      case _ => false
    })
  }

  def parseDepths(input: String): Seq[Int] = input.linesIterator.map(_.toInt).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countIncreases(parseDepths(input)))
  }
}
