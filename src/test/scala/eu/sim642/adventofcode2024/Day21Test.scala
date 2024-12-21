package eu.sim642.adventofcode2024

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  val exampleInput =
    """029A
      |980A
      |179A
      |456A
      |379A""".stripMargin

  test("Part 1 examples") {
    assert(sumCodeComplexity(parseCodes(exampleInput)) == 126384)
  }

  test("Part 1 input") {
    assert(sumCodeComplexity(parseCodes(input)) == 157892)
  }
}
