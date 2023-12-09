package eu.sim642.adventofcode2016

object Day9 {

  private val markerRegex = """\((\d+)x(\d+)\)""".r

  trait Part {
    def decompressedLength(str: String): Long
  }

  object Part1 extends Part {
    override def decompressedLength(str: String): Long = {
      markerRegex.findFirstMatchIn(str) match {
        case Some(m) =>
          val markerLength = m.group(1).toInt
          val markerTimes = m.group(2).toInt
          m.start + markerTimes * markerLength + decompressedLength(m.after.toString.substring(markerLength))
        case None =>
          str.length
      }
    }
  }

  object Part2 extends Part {
    override def decompressedLength(str: String): Long = {
      markerRegex.findFirstMatchIn(str) match {
        case Some(m) =>
          val markerLength = m.group(1).toInt
          val markerTimes = m.group(2).toInt
          val afterStr = m.after.toString
          val markerStr = afterStr.substring(0, markerLength)
          m.start + markerTimes * decompressedLength(markerStr) + decompressedLength(afterStr.substring(markerLength))
        case None =>
          str.length
      }
    }
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.decompressedLength(input))
    println(Part2.decompressedLength(input))
  }
}
