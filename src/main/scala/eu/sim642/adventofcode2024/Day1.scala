package eu.sim642.adventofcode2024

object Day1 {

  def totalListDistance(lists: Seq[(Int, Int)]): Int = {
    val (list1, list2) = lists.unzip
    (list1.sorted lazyZip list2.sorted)
      .map((i, j) => (i - j).abs)
      .sum
  }

  def parsePair(s: String): (Int, Int) = s match {
    case s"$i   $j" => (i.toInt, j.toInt)
  }

  def parseLists(input: String): Seq[(Int, Int)] = input.linesIterator.map(parsePair).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalListDistance(parseLists(input)))
  }
}
