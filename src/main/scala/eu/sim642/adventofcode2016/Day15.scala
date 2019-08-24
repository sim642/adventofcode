package eu.sim642.adventofcode2016

import scala.annotation.tailrec

import eu.sim642.adventofcodelib.IntegralImplicits._

object Day15 {

  case class Disk(i: Int, posCount: Int, initialPos: Int)

  // position + t + disk# = 0 (mod #positions)
  // 4 + t + 1 = 0 (mod 5)
  // 1 + t + 2 = 0 (mod 2)

  // t = 0 (mod 5)
  // t = 1 (mod 2)
  // t = 5

  // TODO: move gcd things to library
  def extendedGcd(a: Int, b: Int): ((Int, Int), Int, (Int, Int)) = {

    @tailrec
    def helper(s: Int, oldS: Int, t: Int, oldT: Int, r: Int, oldR: Int): ((Int, Int), Int, (Int, Int)) = {
      if (r == 0)
        ((oldS, oldT), oldR, (s, t))
      else {
        val q = oldR / r
        helper(oldS - q * s, s, oldT - q * t, t, oldR - q * r, r)
      }
    }

    helper(0, 1, 1, 0, b, a)
  }

  def bezoutCoefs(a: Int, b: Int): (Int, Int) = extendedGcd(a, b)._1

  def crt2(an1: (Int, Int), an2: (Int, Int)): (Int, Int) = {
    val (a1, n1) = an1
    val (a2, n2) = an2
    val (m1, m2) = bezoutCoefs(n1, n2)
    val N = n1 * n2
    // TODO: remove overflow avoiding hack, generalize to Numeric?
    val x = a1.toLong * m2 * n2 + a2 * m1 * n1
    ((x %+ N.toLong).toInt, N)
  }

  def crt(ans: Seq[(Int, Int)]): (Int, Int) = ans.reduce(crt2)

  trait Part {
    def firstPressTime(disks: Seq[Disk]): Int

    def firstPressTime(input: String): Int = firstPressTime(parseInput(input))
  }

  object Part1 extends Part {
    override def firstPressTime(disks: Seq[Disk]): Int = {
      val ans = disks.map({ case Disk(i, posCount, initialPos) => ((-(initialPos + i)) %+ posCount, posCount) })
      crt(ans)._1
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

  def parseInput(input: String): Seq[Disk] = input.lines.map(parseDisk).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.firstPressTime(input))
    println(Part2.firstPressTime(input))

    // part 1: 577731526 - too high
    // part 2: 3180542 - too low
    // part 2: 3302376 - too high
  }
}
