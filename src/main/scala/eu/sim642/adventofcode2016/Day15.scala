package eu.sim642.adventofcode2016

import eu.sim642.adventofcodelib.IntegralImplicits._
import eu.sim642.adventofcodelib.NumberTheory

object Day15 {

  case class Disk(i: Int, posCount: Int, initialPos: Int)

  // position + t + disk# = 0 (mod #positions)
  // 4 + t + 1 = 0 (mod 5)
  // 1 + t + 2 = 0 (mod 2)

  // t = 0 (mod 5)
  // t = 1 (mod 2)
  // t = 5

  trait Part {
    def firstPressTime(disks: Seq[Disk]): Int

    def firstPressTime(input: String): Int = firstPressTime(parseInput(input))
  }

  object Part1 extends Part {
    override def firstPressTime(disks: Seq[Disk]): Int = {
      val ans = disks.map({ case Disk(i, posCount, initialPos) => ((-(initialPos + i)) %+ posCount, posCount) })
      NumberTheory.crt(ans)._1
    }
  }

  object Part2 extends Part {
    override def firstPressTime(disks: Seq[Disk]): Int = {
      val extraDisk = Disk(disks.size + 1, 11, 0)
      Part1.firstPressTime(extraDisk +: disks)
    }
  }

  private val diskRegex = """Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).""".r

  def parseDisk(s: String): Disk = s match {
    case diskRegex(i, posCount, initialPos) => Disk(i.toInt, posCount.toInt, initialPos.toInt)
  }

  def parseInput(input: String): Seq[Disk] = input.linesIterator.map(parseDisk).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.firstPressTime(input))
    println(Part2.firstPressTime(input))

    // part 1: 577731526 - too high
    // part 2: 3180542 - too low
    // part 2: 3302376 - too high
  }
}
