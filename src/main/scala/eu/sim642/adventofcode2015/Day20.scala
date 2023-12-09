package eu.sim642.adventofcode2015

import scala.collection.mutable

object Day20 {

  private val maxHouse = 1000000

  def findHouse(atLeast: Int): Int = {
    val presents = mutable.Seq.fill(maxHouse)(0)
    for (elf <- 1 until presents.size) {
      for (house <- elf until presents.size by elf) {
        presents(house) += elf
      }
    }
    presents.indexWhere(_ * 10 >= atLeast)
  }

  def findHouseLimit(atLeast: Int, limit: Int = 50): Int = {
    val presents = mutable.Seq.fill(maxHouse)(0)
    for (elf <- 1 until presents.size) {
      for (house <- (elf until presents.size by elf).take(limit)) {
        presents(house) += elf
      }
    }
    presents.indexWhere(_ * 11 >= atLeast)
  }

  //lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day20.txt")).mkString.trim
  val input = 33100000

  def main(args: Array[String]): Unit = {
    println(findHouse(input))
    println(findHouseLimit(input))
  }
}
