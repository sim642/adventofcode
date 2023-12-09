package eu.sim642.adventofcode2015

object Day5 {

  trait Part {
    def isNice(s: String): Boolean

    def countNice(ss: Seq[String]): Int = ss.count(isNice)
    def countNice(input: String): Int = countNice(input.linesIterator.toSeq)
  }

  object Part1 extends Part {
    private val naughtyStrings = Set("ab", "cd", "pq", "xy")

    override def isNice(s: String): Boolean = {
      // TODO: regex would be faster here?
      val cond1 = s.count("aeiou".contains(_)) >= 3
      val cond2 = (s lazyZip s.tail).exists({ case (c1, c2) => c1 == c2 })
      val cond3 = naughtyStrings.forall(!s.contains(_))
      cond1 && cond2 && cond3
    }
  }

  object Part2 extends Part {
    private val xyxyRegex = """(..).*\1""".r
    private val xyxRegex = """(.).\1""".r

    override def isNice(s: String): Boolean = {
      xyxyRegex.findFirstIn(s).isDefined && xyxRegex.findFirstIn(s).isDefined
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day5.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countNice(input))
    println(Part2.countNice(input))
  }
}
