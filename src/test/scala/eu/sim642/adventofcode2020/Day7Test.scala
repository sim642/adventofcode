package eu.sim642.adventofcode2020

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  val exampleInput =
    """light red bags contain 1 bright white bag, 2 muted yellow bags.
      |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
      |bright white bags contain 1 shiny gold bag.
      |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
      |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
      |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
      |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
      |faded blue bags contain no other bags.
      |dotted black bags contain no other bags.""".stripMargin

  val exampleInput2 =
    """shiny gold bags contain 2 dark red bags.
      |dark red bags contain 2 dark orange bags.
      |dark orange bags contain 2 dark yellow bags.
      |dark yellow bags contain 2 dark green bags.
      |dark green bags contain 2 dark blue bags.
      |dark blue bags contain 2 dark violet bags.
      |dark violet bags contain no other bags.""".stripMargin

  test("Part 1 examples") {
    assert(countContainingColors(parseRules(exampleInput)) == 4)
  }

  test("Part 1 input answer") {
    assert(countContainingColors(parseRules(input)) == 177)
  }

  test("Part 2 examples") {
    assert(countContainedBags(parseRules(exampleInput)) == 32)
    assert(countContainedBags(parseRules(exampleInput2)) == 126)
  }

  test("Part 2 input answer") {
    assert(countContainedBags(parseRules(input)) == 34988)
  }
}
