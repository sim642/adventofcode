package eu.sim642.adventofcode2020

import Day19._
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  val exampleRules =
    """0: 1 2
      |1: "a"
      |2: 1 3 | 3 1
      |3: "b"""".stripMargin

  val exampleRules2 =
    """0: 4 1 5
      |1: 2 3 | 3 2
      |2: 4 4 | 5 5
      |3: 4 5 | 5 4
      |4: "a"
      |5: "b"""".stripMargin

  val exampleInput =
    """0: 4 1 5
      |1: 2 3 | 3 2
      |2: 4 4 | 5 5
      |3: 4 5 | 5 4
      |4: "a"
      |5: "b"
      |
      |ababbb
      |bababa
      |abbbab
      |aaabbb
      |aaaabbb""".stripMargin

  test("parseRules") {
    println(parseRules(exampleRules))
    println(parseRules(exampleRules2))
  }

  test("parseInput") {
    println(parseInput(exampleInput))
    println(parseInput(input))
  }

  test("Part 1 examples") {
    assert(countMatchingMessages(parseInput(exampleInput)) == 2)
  }

  test("Part 1 input answer") {
    assert(countMatchingMessages(parseInput(input)) == 226)
  }
}
