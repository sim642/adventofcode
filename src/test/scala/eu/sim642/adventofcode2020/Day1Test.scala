package eu.sim642.adventofcode2020

import Day1._
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends AnyFunSuite {

  val exampleInput =
    """1721
      |979
      |366
      |299
      |675
      |1456""".stripMargin

  test("Part 1 examples") {
    assert(entryProduct2020(parseEntries(exampleInput)) == 514579)
  }

  test("Part 1 input answer") {
    assert(entryProduct2020(parseEntries(input)) == 713184)
  }
}
