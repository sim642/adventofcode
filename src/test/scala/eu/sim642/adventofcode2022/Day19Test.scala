package eu.sim642.adventofcode2022

import Day19._
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  val exampleInput =
    """Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
      |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.""".stripMargin

  test("parseBlueprints") {
    parseBlueprints(exampleInput)
    parseBlueprints(input)
  }

  test("Part 1 examples") {
    val exampleBlueprints = parseBlueprints(exampleInput)

    assert(maxGeodes(exampleBlueprints.head, 24) == 9)
    assert(maxGeodes(exampleBlueprints(1), 24) == 12)
    assert(sumQualityLevel(exampleBlueprints) == 33)
  }

  test("Part 1 input answer") {
    assert(sumQualityLevel(parseBlueprints(input)) == 1487)
  }

  test("Part 2 examples") {
    val exampleBlueprints = parseBlueprints(exampleInput)

    assert(maxGeodes(exampleBlueprints.head, 32) == 56)
    assert(maxGeodes(exampleBlueprints(1), 32) == 62)
    assert(productMaxGeodes(exampleBlueprints) == 56 * 62)
  }

  test("Part 2 input answer") {
    assert(productMaxGeodes(parseBlueprints(input)) == 13440)
  }
}
