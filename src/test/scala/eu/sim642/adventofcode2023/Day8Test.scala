package eu.sim642.adventofcode2023

import Day8._
import org.scalatest.funsuite.AnyFunSuite

class Day8Test extends AnyFunSuite {

  private val exampleInput =
    """RL
      |
      |AAA = (BBB, CCC)
      |BBB = (DDD, EEE)
      |CCC = (ZZZ, GGG)
      |DDD = (DDD, DDD)
      |EEE = (EEE, EEE)
      |GGG = (GGG, GGG)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  private val exampleInput2 =
    """LLR
      |
      |AAA = (BBB, BBB)
      |BBB = (AAA, ZZZ)
      |ZZZ = (ZZZ, ZZZ)""".stripMargin

  test("Part 1 examples") {
    assert(followInstructionsSteps(parseInput(exampleInput)) == 2)
    assert(followInstructionsSteps(parseInput(exampleInput2)) == 6)
  }

  test("Part 1 input answer") {
    assert(followInstructionsSteps(parseInput(input)) == 21389)
  }
}
