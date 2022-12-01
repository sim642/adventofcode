package eu.sim642.adventofcode2022

object Day1 {

  type Elf = Seq[Int]

  def maxElfTotal(elves: Seq[Elf]): Int = elves.map(_.sum).max

  def parseElf(s: String): Elf = s.linesIterator.map(_.toInt).toSeq

  def parseElves(input: String): Seq[Elf] = input.split("\n\n").toSeq.map(parseElf)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(maxElfTotal(parseElves(input)))
  }
}
