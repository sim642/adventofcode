package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day3.Pos

object Day11 {

  def hundredsDigit(n: Int): Int = n / 100 % 10

  def powerLevel(serialNumber: Int, pos: Pos): Int = {
    val rackID = pos.x + 10
    hundredsDigit((rackID * pos.y + serialNumber) * rackID) - 5
  }

  val offsets: Seq[Pos] =
    for {
      y <- 0 to 2
      x <- 0 to 2
    } yield Pos(x, y)

  def powerLevelSquare(serialNumber: Int, topLeft: Pos): Int = {
    offsets.map(topLeft + _).map(powerLevel(serialNumber, _)).sum
  }

  def largestPowerLevelSquare(serialNumber: Int): Pos = {
    (for {
      y <- (1 until (300 - 2)).toIterator
      x <- (1 until (300 - 2)).toIterator
    } yield Pos(x, y)).maxBy(powerLevelSquare(serialNumber, _))
  }

  def largestPowerLevelSquareString(serialNumber: Int): String = {
    val pos = largestPowerLevelSquare(serialNumber)
    s"${pos.x},${pos.y}"
  }


  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim
  val input = 7403

  def main(args: Array[String]): Unit = {
    println(largestPowerLevelSquareString(input))
  }
}
