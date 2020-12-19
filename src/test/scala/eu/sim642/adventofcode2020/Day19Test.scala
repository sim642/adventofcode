package eu.sim642.adventofcode2020

import Day19._
import Day19Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends Suites(
  new BaseTest,
  new RegexSolutionTest,
  new ParserSolutionTest,
  new EarleySolutionTest,
)

object Day19Test {

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

  val exampleInput2 =
    """42: 9 14 | 10 1
      |9: 14 27 | 1 26
      |10: 23 14 | 28 1
      |1: "a"
      |11: 42 31
      |5: 1 14 | 15 1
      |19: 14 1 | 14 14
      |12: 24 14 | 19 1
      |16: 15 1 | 14 14
      |31: 14 17 | 1 13
      |6: 14 14 | 1 14
      |2: 1 24 | 14 4
      |0: 8 11
      |13: 14 3 | 1 12
      |15: 1 | 14
      |17: 14 2 | 1 7
      |23: 25 1 | 22 14
      |28: 16 1
      |4: 1 1
      |20: 14 14 | 1 15
      |3: 5 14 | 16 1
      |27: 1 6 | 14 18
      |14: "b"
      |21: 14 1 | 1 14
      |25: 1 1 | 1 14
      |22: 14 14
      |8: 42
      |26: 14 22 | 1 20
      |18: 15 15
      |7: 14 5 | 1 21
      |24: 14 1
      |
      |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
      |bbabbbbaabaabba
      |babbbbaabbbbbabbbbbbaabaaabaaa
      |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
      |bbbbbbbaaaabbbbaaabbabaaa
      |bbbababbbbaaaaaaaabbababaaababaabab
      |ababaaaaaabaaab
      |ababaaaaabbbaba
      |baabbaaaabbaaaababbaababb
      |abbbbabbbbaaaababbbbbbaaaababb
      |aaaaabbaabaaaaababaa
      |aaaabbaaaabbaaa
      |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
      |babaaabbbaaabaababbaabababaaab
      |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba""".stripMargin

  class BaseTest extends AnyFunSuite {

    test("parseRules") {
      parseRules(exampleRules)
      parseRules(exampleRules2)
    }

    test("parseInput") {
      parseInput(exampleInput)
      parseInput(input)
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

    test("Part 1 examples") {
      assert(solution.countMatchingMessages(parseInput(exampleInput)) == 2)
    }

    test("Part 1 input answer") {
      assert(solution.countMatchingMessages(parseInput(input)) == 226)
    }

    protected val testPart2: Boolean = true

    if (testPart2) {
      test("Part 2 examples") {
        assert(solution.countMatchingMessages(parseInput(exampleInput2)) == 3)
        assert(solution.countMatchingMessagesFixed(parseInput(exampleInput2)) == 12)
      }

      test("Part 2 input answer") {
        assert(solution.countMatchingMessagesFixed(parseInput(input)) == 355)
      }
    }
  }

  class RegexSolutionTest extends SolutionTest(RegexSolution) {
    override protected val testPart2: Boolean = false
  }

  class ParserSolutionTest extends SolutionTest(ParserSolution) {
    override protected val testPart2: Boolean = false // TODO: fix solution
  }

  class EarleySolutionTest extends SolutionTest(EarleySolution)
}
