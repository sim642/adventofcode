package eu.sim642.adventofcode2016

import scala.annotation.tailrec

object Day15 {

  implicit class ModPos(n: Int) {
    def %+(d: Int): Int = (n % d + d) % d
  }

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
    val x = a1 * m2 * n2 + a2 * m1 * n1
    (x %+ N, N)
  }

  def crt(ans: Seq[(Int, Int)]): (Int, Int) = ans.reduce(crt2)

  def firstPressTime(disks: Seq[Disk]): Int = {
    val ans = disks.map({ case Disk(i, posCount, initialPos) => ((-(initialPos + i)) %+ posCount, posCount)})
    crt(ans)._1
  }

  private val diskRegex = """Disc #(\d+) has (\d+) positions; at time=0, it is at position (\d+).""".r

  def parseDisk(s: String): Disk = s match {
    case diskRegex(i, posCount, initialPos) => Disk(i.toInt, posCount.toInt, initialPos.toInt)
  }

  def parseInput(input: String): Seq[Disk] = input.lines.map(parseDisk).toSeq

  def firstPressTime(input: String): Int = firstPressTime(parseInput(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(firstPressTime(input))

    // 577731526 - too high
  }
}
