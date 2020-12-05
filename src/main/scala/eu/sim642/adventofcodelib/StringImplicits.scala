package eu.sim642.adventofcodelib

object StringImplicits {

  implicit class ParseRadixOps(s: String) {
    def toInt(radix: Int): Int = Integer.parseInt(s, radix)
  }
}
