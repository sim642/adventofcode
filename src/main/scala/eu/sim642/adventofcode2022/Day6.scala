package eu.sim642.adventofcode2022

object Day6 {

  def startOfPacketIndex(datastream: String, length: Int): Int = {
    datastream
      .view
      .sliding(length)
      .indexWhere(group => group.toSet.size == length) + length
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(startOfPacketIndex(input, 4))
    println(startOfPacketIndex(input, 14))
  }
}
