package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day3.Pos

import scala.collection.mutable

object Day11 {

  def hundredsDigit(n: Int): Int = n / 100 % 10

  def powerLevel(serialNumber: Int)(pos: Pos): Int = {
    val rackID = pos.x + 10
    hundredsDigit((rackID * pos.y + serialNumber) * rackID) - 5
  }

  trait SumGrid {
    def sumRect(topLeft: Pos, bottomRight: Pos): Int
  }

  class NaiveSumGrid(f: Pos => Int) extends SumGrid {
    override def sumRect(topLeft: Pos, bottomRight: Pos): Int = {
      (for {
        y <- topLeft.y to bottomRight.y
        x <- topLeft.x to bottomRight.x
        pos = Pos(x, y)
      } yield f(pos)).sum
    }
  }

  class PartialSumGrid(f: Pos => Int, min: Pos, max: Pos) extends SumGrid {
    val partialSums: mutable.Map[Pos, Int] = mutable.Map.empty.withDefaultValue(0)

    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val pos = Pos(x, y)
        val sum = partialSums(pos + Pos(0, -1)) + partialSums(pos + Pos(-1, 0)) - partialSums(pos + Pos(-1, -1)) + f(pos)
        partialSums.put(pos, sum)
      }
    }

    override def sumRect(topLeft: Pos, bottomRight: Pos): Int = {
      partialSums(bottomRight) - partialSums(Pos(topLeft.x - 1, bottomRight.y)) - partialSums(Pos(bottomRight.x, topLeft.y - 1)) + partialSums(Pos(topLeft.x - 1, topLeft.y - 1))
    }
  }

  def largestPowerLevelSquare(serialNumber: Int): Pos = {
    val sumGrid = new PartialSumGrid(powerLevel(serialNumber), Pos(1, 1), Pos(300, 300))

    (for {
      y <- (1 until (300 - 2)).toIterator
      x <- (1 until (300 - 2)).toIterator
    } yield Pos(x, y)).maxBy(p => sumGrid.sumRect(p, p + Pos(2, 2)))
  }

  def largestPowerLevelSquareString(serialNumber: Int): String = {
    val pos = largestPowerLevelSquare(serialNumber)
    s"${pos.x},${pos.y}"
  }

  def largestPowerLevelSquare2(serialNumber: Int): (Pos, Int) = {
    val sumGrid = new PartialSumGrid(powerLevel(serialNumber), Pos(1, 1), Pos(300, 300))

    (for {
      size <- (1 to 300).toIterator
      y <- (1 until (300 - (size - 1))).toIterator
      x <- (1 until (300 - (size - 1))).toIterator
    } yield (Pos(x, y), size)).maxBy(p => sumGrid.sumRect(p._1, p._1 + Pos(p._2 - 1, p._2 - 1)))
  }

  def largestPowerLevelSquareString2(serialNumber: Int): String = {
    val (pos, size) = largestPowerLevelSquare2(serialNumber)
    s"${pos.x},${pos.y},$size"
  }


  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim
  val input = 7403

  def main(args: Array[String]): Unit = {
    println(largestPowerLevelSquareString(input))
    println(largestPowerLevelSquareString2(input))
  }
}
