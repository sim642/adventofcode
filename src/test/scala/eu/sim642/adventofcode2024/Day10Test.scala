package eu.sim642.adventofcode2024

import Day10._
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends AnyFunSuite {

  val exampleInput =
    """0123
      |1234
      |8765
      |9876""".stripMargin

  val exampleInput2 =
    """89010123
      |78121874
      |87430965
      |96549874
      |45678903
      |32019012
      |01329801
      |10456732""".stripMargin

  test("Part 1 examples") {
    assert(sumTrailheadScores(parseGrid(exampleInput2)) == 36)
  }

  test("Part 1 input answer") {
    assert(sumTrailheadScores(parseGrid(input)) == 557)
  }

  test("Part 2 examples") {
    assert(sumTrailheadRatings(parseGrid(exampleInput2)) == 81)
  }

  test("Part 2 input answer") {
    assert(sumTrailheadRatings(parseGrid(input)) == 1062)
  }
}
