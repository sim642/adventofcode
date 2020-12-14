package eu.sim642.adventofcodelib

object StringImplicits {

  implicit class ParseRadixOps(s: String) {
    def toIntRadix(radix: Int): Int = Integer.parseInt(s, radix)

    def toLongRadix(radix: Int): Long = java.lang.Long.parseLong(s, radix)
  }
}
