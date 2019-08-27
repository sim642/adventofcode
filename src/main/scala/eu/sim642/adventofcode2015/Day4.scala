package eu.sim642.adventofcode2015

import eu.sim642.adventofcode2016.Day14.md5

object Day4 {

  def findZeroHash(input: String): Int = {
    Iterator.from(1).find(i => md5(input + i).startsWith("00000")).get
  }


  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim
  val input = "ckczppom"

  def main(args: Array[String]): Unit = {
    println(findZeroHash(input))
  }
}
