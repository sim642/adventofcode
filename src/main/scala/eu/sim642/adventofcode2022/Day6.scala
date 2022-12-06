package eu.sim642.adventofcode2022

object Day6 {

  def startOfPacketIndex(datastream: String): Int = {
    datastream
      .view
      .sliding(4)
      .indexWhere(group => group.toSet.size == 4) + 4
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(startOfPacketIndex(input))
  }
}
