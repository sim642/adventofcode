package eu.sim642.adventofcode2016

object Day6 {

  def errorCorrect(messages: Seq[String]): String = {
    messages.transpose.map(_.groupBy(c => c).mapValues(_.size).maxBy(_._2)._1).mkString("")
  }

  def errorCorrect(input: String): String = errorCorrect(input.lines.toSeq)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(errorCorrect(input))
  }
}
