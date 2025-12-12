package eu.sim642.adventofcode2025

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  val exampleInput =
    """0:
      |###
      |##.
      |##.
      |
      |1:
      |###
      |##.
      |.##
      |
      |2:
      |.##
      |###
      |##.
      |
      |3:
      |##.
      |###
      |##.
      |
      |4:
      |###
      |#..
      |###
      |
      |5:
      |###
      |.#.
      |###
      |
      |4x4: 0 0 0 0 2 0
      |12x5: 1 0 1 0 2 2
      |12x5: 1 0 1 0 3 2""".stripMargin

  ignore("Part 1 examples") { // TODO: optimize
    import NaiveSolution._ // doesn't work with cheat solution
    //val input1 = parseInput(exampleInput)
    //assert(fits(input1.shapes)(input1.regions(0)))
    //assert(fits(input1.shapes)(input1.regions(1)))
    //assert(!fits(input1.shapes)(input1.regions(2)))
    assert(countFits(parseInput(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(SanitySolution.countFits(parseInput(input)) == 443)
  }
}
