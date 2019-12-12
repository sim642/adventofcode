package eu.sim642.adventofcodelib

import scala.annotation.tailrec
import scala.math.Integral.Implicits._
import eu.sim642.adventofcodelib.IntegralImplicits._

object NumberTheory {

  def extendedGcd[A](a: A, b: A)(implicit aIntegral: Integral[A]): ((A, A), A, (A, A)) = {

    @tailrec
    def helper(s: A, oldS: A, t: A, oldT: A, r: A, oldR: A): ((A, A), A, (A, A)) = {
      if (r == 0)
        ((oldS, oldT), oldR, (s, t))
      else {
        val q = oldR / r
        helper(oldS - q * s, s, oldT - q * t, t, oldR - q * r, r)
      }
    }

    helper(aIntegral.zero, aIntegral.one, aIntegral.one, aIntegral.zero, b, a)
  }

  def gcd[A: Integral](a: A, b: A): A = extendedGcd(a, b)._2

  def gcd[A: Integral](as: Seq[A]): A = as.reduce(gcd(_, _))

  def lcm[A: Integral](a: A, b: A): A = a / gcd(a, b) * b // divide before multiply to reduce overflow risk

  def lcm[A: Integral](as: Seq[A]): A = as.reduce(lcm(_, _))

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
