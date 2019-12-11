package eu.sim642.adventofcodelib

object FractionalImplicits {

  implicit class ExtraModFloatOps(n: Float) {
    def %+(d: Float): Float = (n % d + d) % d
  }

  implicit class ExtraModDoubleOps(n: Double) {
    def %+(d: Double): Double = (n % d + d) % d
  }
}
