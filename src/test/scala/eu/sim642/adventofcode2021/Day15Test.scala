package eu.sim642.adventofcode2021

import Day15._
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {

  val exampleInput =
    """1163751742
      |1381373672
      |2136511328
      |3694931569
      |7463417111
      |1319128137
      |1359912421
      |3125421639
      |1293138521
      |2311944581""".stripMargin

  test("Part 1 examples") {
    assert(lowestRiskPath(parseGrid(exampleInput)) == 40)
  }

  test("Part 1 input answer") {
    assert(lowestRiskPath(parseGrid(input)) == 811)
  }
}
