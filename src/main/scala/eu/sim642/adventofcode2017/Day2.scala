package eu.sim642.adventofcode2017

object Day2 {

  def rowChecksum(row: Seq[Int]): Int = row.max - row.min

  def checksum(spreadsheet: Seq[Seq[Int]]): Int = spreadsheet.map(rowChecksum).sum

  def checksum(s: String): Int = {
    val spreadsheet = s.trim.split('\n').toSeq.map(_.trim.split("\\s+").toSeq.map(_.toInt))
    checksum(spreadsheet)
  }

  val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day2.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(checksum(input))
  }
}
