package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.Hash

object Day4 {

  trait Part {
    protected val prefix: String

    def findZeroHash(input: String): Int = {
      Iterator.from(1).find(i => Hash.md5(input + i).startsWith(prefix)).get
    }
  }

  object Part1 extends Part {
    override protected val prefix: String = "00000"
  }

  object Part2 extends Part {
    override protected val prefix: String = "000000"
  }


  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim
  val input = "ckczppom"

  def main(args: Array[String]): Unit = {
    println(Part1.findZeroHash(input))
    println(Part2.findZeroHash(input))
  }
}
