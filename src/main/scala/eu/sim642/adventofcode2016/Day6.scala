package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2018.Day2.GroupCountIterable

object Day6 {

  trait Part {
    def errorCorrect(messages: Seq[String]): String

    def errorCorrect(input: String): String = errorCorrect(input.linesIterator.toSeq)
  }

  object Part1 extends Part {
    override def errorCorrect(messages: Seq[String]): String = {
      messages.transpose.map(_.groupCount(identity).maxBy(_._2)._1).mkString("")
    }
  }

  object Part2 extends Part {
    override def errorCorrect(messages: Seq[String]): String = {
      messages.transpose.map(_.groupCount(identity).minBy(_._2)._1).mkString("")
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.errorCorrect(input))
    println(Part2.errorCorrect(input))
  }
}
