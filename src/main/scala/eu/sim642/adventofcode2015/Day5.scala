package eu.sim642.adventofcode2015

object Day5 {

  private val naughtyStrings = Set("ab", "cd", "pq", "xy")

  def isNice(s: String): Boolean = {
    val cond1 = s.count("aeiou".contains(_)) >= 3
    val cond2 = (s lazyZip s.tail).exists({ case (c1, c2) => c1 == c2 })
    val cond3 = naughtyStrings.forall(!s.contains(_))
    cond1 && cond2 && cond3
  }

  def countNice(ss: Seq[String]): Int = ss.count(isNice)

  def countNice(input: String): Int = countNice(input.linesIterator.toSeq)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countNice(input))
  }
}
