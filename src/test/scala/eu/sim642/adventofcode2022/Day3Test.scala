package eu.sim642.adventofcode2022

import Day3._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  val exampleInput =
    """vJrwpWtwJgWrhcsFMMfFFhFp
      |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
      |PmmdzqPrVvPwwTWBwg
      |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
      |ttgJtRGJQctTZtZT
      |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  test("Part 1 examples") {
    assert(commonItemPrioritySum(parseRucksacks(exampleInput)) == 157)
  }

  test("Part 1 input answer") {
    assert(commonItemPrioritySum(parseRucksacks(input)) == 7742)
  }
}
