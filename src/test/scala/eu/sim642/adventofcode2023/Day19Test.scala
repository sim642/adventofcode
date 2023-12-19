package eu.sim642.adventofcode2023

import Day19._
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  private val exampleInput =
    """px{a<2006:qkq,m>2090:A,rfg}
      |pv{a>1716:R,A}
      |lnx{m>1548:A,A}
      |rfg{s<537:gd,x>2440:R,A}
      |qs{s>3448:A,lnx}
      |qkq{x<1416:A,crn}
      |crn{x>2662:A,R}
      |in{s<1351:px,qqz}
      |qqz{s>2770:qs,m<1801:hdj,R}
      |gd{a>3333:R,R}
      |hdj{m>838:A,pv}
      |
      |{x=787,m=2655,a=1222,s=2876}
      |{x=1679,m=44,a=2067,s=496}
      |{x=2036,m=264,a=79,s=2244}
      |{x=2461,m=1339,a=466,s=291}
      |{x=2127,m=1623,a=2188,s=1013}""".stripMargin

  test("Part 1 examples") {
    assert(totalAcceptedRating(parseInput(exampleInput)) == 19114)
  }

  test("Part 1 input answer") {
    assert(totalAcceptedRating(parseInput(input)) == 418498)
  }

  test("Part 2 examples") {
    assert(countAllAccepted(parseInput(exampleInput)) == 167409079868000L)
  }

  test("Part 2 input answer") {
    assert(countAllAccepted(parseInput(input)) == 123331556462603L)
  }
}
