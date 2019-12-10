package eu.sim642.adventofcodelib

import scala.annotation.tailrec
import eu.sim642.adventofcodelib.IntegralImplicits._

object NumberTheory {

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

  def gcd(a: Int, b: Int): Int = extendedGcd(a, b)._2

  def bezoutCoefs(a: Int, b: Int): (Int, Int) = extendedGcd(a, b)._1

  private def crt2(an1: (Int, Int), an2: (Int, Int)): (Int, Int) = {
    val (a1, n1) = an1
    val (a2, n2) = an2
    val (m1, m2) = bezoutCoefs(n1, n2)
    val N = n1 * n2
    // TODO: remove overflow avoiding hack, generalize to Numeric?
    val x = a1.toLong * m2 * n2 + a2 * m1 * n1
    ((x %+ N.toLong).toInt, N)
  }

  def crt(ans: Seq[(Int, Int)]): (Int, Int) = ans.reduce(crt2)
}
