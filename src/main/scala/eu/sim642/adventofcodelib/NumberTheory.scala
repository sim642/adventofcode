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

  def bezoutCoefs[A: Integral](a: A, b: A): (A, A) = extendedGcd(a, b)._1

  def modInv[A: Integral](a: A, m: A): A = bezoutCoefs(a, m)._1 %+ m

  private def crt2[A: Integral](an1: (A, A), an2: (A, A)): (A, A) = {
    // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Existence_(constructive_proof)
    // https://en.wikipedia.org/wiki/Chinese_remainder_theorem#Generalization_to_non-coprime_moduli
    // https://forthright48.com/chinese-remainder-theorem-part-2-non-coprime-moduli/
    val (a1, n1) = an1
    val (a2, n2) = an2
    val ((m1, m2), g, _) = extendedGcd(n1, n2)
    if ((a1 - a2) % g != 0)
      throw new NoSuchElementException("contradictory CRT constraints") // TODO: use Option return value instead
    val N = n1 / g * n2 // lcm(n1, n2)
    // TODO: avoid overflow if possible
    //val x = (a1 * m2 * n2 + a2 * m1 * n1) / g
    val x = a1 * m2 * (n2 / g) + a2 * m1 * (n1 / g)
    (x %+ N, N)
  }

  def crt[A: Integral](ans: Seq[(A, A)]): (A, A) = ans.reduce(crt2(_, _))
}
