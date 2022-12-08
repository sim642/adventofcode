package eu.sim642.adventofcode2022

import Day8._
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite {

  val exampleInput =
    """30373
      |25512
      |65332
      |33549
      |35390""".stripMargin

  test("Part 1 examples") {
    assert(countVisibleTrees(parseGrid(exampleInput)) == 21)
  }

  test("Part 1 input answer") {
    assert(countVisibleTrees(parseGrid(input)) == 1713)
  }
}
