package eu.sim642.mits2024

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """Kelluke: 1.64
      |ForMe: 1.89
      |Dynamit: 0.92
      |odForMedFuDrDynamiti""".stripMargin

  test("Example") {
    assert(parseInput(exampleInput).orderTotal == 2.81)
  }

  test("Input answer") {
    assert(parseInput(input).orderTotal ==
      74.64)
  }
}
