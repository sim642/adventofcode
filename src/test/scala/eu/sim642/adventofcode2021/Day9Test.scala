package eu.sim642.adventofcode2021

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  val exampleInput =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin

  test("Part 1 examples") {
    assert(sumLowPointRiskLevels(parseGrid(exampleInput)) == 15)
  }

  test("Part 1 input answer") {
    assert(sumLowPointRiskLevels(parseGrid(input)) == 532)
  }
}
