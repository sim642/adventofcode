package eu.sim642.adventofcode2016

object Day9 {

  private val markerRegex = """\((\d+)x(\d+)\)""".r

  def decompressedLength(str: String): Int = {
    markerRegex.findFirstMatchIn(str) match {
      case Some(m) =>
        val markerLength = m.group(1).toInt
        val markerTimes = m.group(2).toInt
        m.start + markerTimes * markerLength + decompressedLength(m.after.toString.substring(markerLength))
      case None =>
        str.length
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(decompressedLength(input))
  }
}
