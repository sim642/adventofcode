package eu.sim642.adventofcode2022

object Day4 {

  // TODO: move Interval to library (1D Box)
  case class Interval(min: Int, max: Int) {
    def contains(that: Interval): Boolean = this.min <= that.min && that.max <= this.max
    def overlaps(that: Interval): Boolean = this.min <= that.max && that.min <= this.max
  }

  type Pair = (Interval, Interval)

  def countFullyContained(pairs: Seq[Pair]): Int = {
    pairs.count({ case (a, b) =>
      a.contains(b) || b.contains(a)
    })
  }

  def countOverlapping(pairs: Seq[Pair]): Int = {
    pairs.count({ case (a, b) =>
      a.overlaps(b)
    })
  }


  private val pairRegex = """(\d+)-(\d+),(\d+)-(\d+)""".r

  def parsePair(s: String): Pair = s match {
    case pairRegex(min1, max1, min2, max2) =>
      (Interval(min1.toInt, max1.toInt), Interval(min2.toInt, max2.toInt))
  }

  def parsePairs(input: String): Seq[Pair] = input.linesIterator.map(parsePair).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countFullyContained(parsePairs(input)))
    println(countOverlapping(parsePairs(input)))
  }
}
