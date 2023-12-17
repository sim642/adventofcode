package eu.sim642.adventofcode2023

import Day17._
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  private val exampleInput =
    """2413432311323
      |3215453535623
      |3255245654254
      |3446585845452
      |4546657867536
      |1438598798454
      |4457876987766
      |3637877979653
      |4654967986887
      |4564679986453
      |1224686865563
      |2546548887735
      |4322674655533""".stripMargin

  private val exampleInput2 =
    """111111111111
      |999999999991
      |999999999991
      |999999999991
      |999999999991""".stripMargin

  test("Part 1 examples") {
    assert(Part1.leastHeatLoss(parseGrid(exampleInput)) == 102)
  }

  test("Part 1 input answer") {
    assert(Part1.leastHeatLoss(parseGrid(input)) == 1110)
  }

  test("Part 2 examples") {
    assert(Part2.leastHeatLoss(parseGrid(exampleInput)) == 94)
    assert(Part2.leastHeatLoss(parseGrid(exampleInput2)) == 71)
  }

  test("Part 2 input answer") {
    assert(Part2.leastHeatLoss(parseGrid(input)) == 1294)
  }
}
