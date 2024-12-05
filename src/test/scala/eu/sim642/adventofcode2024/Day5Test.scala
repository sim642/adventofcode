package eu.sim642.adventofcode2024

import Day5._
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  val exampleInput =
    """47|53
      |97|13
      |97|61
      |97|47
      |75|29
      |61|13
      |75|53
      |29|13
      |97|29
      |53|29
      |61|53
      |97|53
      |61|29
      |47|13
      |75|47
      |97|75
      |47|61
      |75|61
      |47|29
      |75|13
      |53|13
      |
      |75,47,61,53,29
      |97,61,53,29,13
      |75,29,13
      |75,97,47,61,53
      |61,13,29
      |97,13,75,29,47""".stripMargin

  test("Part 1 examples") {
    val input@Input(rules, updates) = parseInput(exampleInput)

    assert(isCorrect(rules, updates(0)))
    assert(isCorrect(rules, updates(1)))
    assert(isCorrect(rules, updates(2)))
    assert(!isCorrect(rules, updates(3)))
    assert(!isCorrect(rules, updates(4)))
    assert(!isCorrect(rules, updates(5)))

    assert(sumCorrectMiddles(input) == 143)
  }

  test("Part 1 input answer") {
    assert(sumCorrectMiddles(parseInput(input)) == 6051)
  }
}
