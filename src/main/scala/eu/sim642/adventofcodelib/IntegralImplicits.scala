package eu.sim642.adventofcodelib

import scala.math.Ordering.Implicits._
import scala.math.Integral.Implicits._

object IntegralImplicits {

  implicit class ExtraDivModIntegralOps[A: Integral](n: A) {
    def %+(d: A): A = (n % d + d) % d

    def /!(d: A): Option[A] = if (n % d == 0) Some(n / d) else None // /% returns both at once but calculates still separately...
  }

  implicit class ModPowIntegralOps[A](a: A)(implicit aIntegral: Integral[A]) {
    def modPow(n: A, m: A): A = {
      // vals for reused constants instead of using Integral defs
      val zero = aIntegral.zero
      val one = aIntegral.one
      val two = aIntegral.fromInt(2)

      // TODO: optimize using tailrec or loop
      def helper(a: A, n: A, m: A): A = {
        if (n equiv zero)
          one
        else {
          val (q, r) = n /% two
          val half = helper(a, q, m)
          val halfSquare = (half * half) % m
          if (r equiv zero)
            halfSquare
          else
            (a * halfSquare) % m
        }
      }

      helper(a, n, m)
    }
  }

  implicit class ExtraDivIntOps(n: Int) {
    def floorDiv(d: Int): Int = Math.floorDiv(n, d)
  }
}
