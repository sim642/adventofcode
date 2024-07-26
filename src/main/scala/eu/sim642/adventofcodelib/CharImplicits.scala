package eu.sim642.adventofcodelib

object CharImplicits {

  implicit class DigitOps(c: Char) {
    def asDigitOption: Option[Int] = Option.when(c.isDigit)(c.asDigit)
  }
}
