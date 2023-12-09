package eu.sim642.adventofcode2015

object Day2 {

  type Present = (Int, Int, Int)

  def wrappingPaperArea(l: Int, w: Int, h: Int): Int = {
    val sideAreas = Seq(l * w, w * h, h * l)
    2 * sideAreas.sum + sideAreas.min
  }

  def totalWrappingPaperArea(presents: Seq[Present]): Int = {
    presents.map((wrappingPaperArea _).tupled).sum
  }

  def totalWrappingPaperArea(input: String): Int = totalWrappingPaperArea(parseInput(input))

  def ribbonLength(l: Int, w: Int, h: Int): Int = {
    val sidePerimeters = Seq(l + w, w + h, h + l).map(2 * _)
    val volume = l * w * h
    sidePerimeters.min + volume
  }

  def totalRibbonLength(presents: Seq[Present]): Int = {
    presents.map((ribbonLength _).tupled).sum
  }

  def totalRibbonLength(input: String): Int = totalRibbonLength(parseInput(input))

  private val presentRegex = """(\d+)x(\d+)x(\d+)""".r

  def parsePresent(s: String): Present = s match {
    case presentRegex(l, w, h) => (l.toInt, w.toInt, h.toInt)
  }

  def parseInput(input: String): Seq[Present] = input.linesIterator.map(parsePresent).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalWrappingPaperArea(input))
    println(totalRibbonLength(input))
  }
}
