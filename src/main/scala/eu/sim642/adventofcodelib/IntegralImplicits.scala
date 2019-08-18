package eu.sim642.adventofcodelib

import scala.math.Integral.Implicits._

object IntegralImplicits {

  implicit class ExtraIntegralOps[A: Integral](n: A) {
    def %+(d: A): A = (n % d + d) % d

    def /!(d: A): Option[A] = if (n % d == 0) Some(n / d) else None // /% returns both at once but calculates still separately...
  }

  implicit class ExtraIntOps(n: Int) {
    def floorDiv(d: Int): Int = Math.floorDiv(n, d)
  }
}
