package eu.sim642.adventofcode2021

import Day10._
import Day10.ParseLineResult._
import Day10Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day10Test extends Suites(
  new BaseTest,
  new ParserCombinatorSolutionTest,
  new StackSolutionTest,
  new RecursiveDescentSolutionTest,
)

object Day10Test {

  val exampleInput =
    """[({(<(())[]>[[{[]{<()<>>
      |[(()[<>])]({[<{<<[]>>(
      |{([(<{}[<>[]}>{[]{[(<()>
      |(((({<>}<{<{<>}{[]{[]{}
      |[[<[([]))<([[{}[[()]]]
      |[{[{({}]{}}([{[{{{}}([]
      |{<[[]]>}<{[{[{[]{()[[[]
      |[<(<(<(<{}))><([]([]()
      |<{([([[(<>()){}]>(<<{{
      |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin

  class BaseTest extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("Part 2 examples") {
      val completionExpectedScore = Table(
        ("completion", "expectedScore"),
        ("}}]])})]", 288957),
        (")}>]})", 5566),
        ("}}>}>))))", 1480781),
        ("]]}}]}]}>", 995444),
        ("])}>", 294),
      )

      forAll(completionExpectedScore) { (completion, expectedScore) =>
        assert(completionScore(completion) == expectedScore)
      }
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite with ScalaCheckPropertyChecks {

    test("Part 1 examples") {
      val legalLines = Table(
        "line",
        "()",
        "[]",
        "([])",
        "{()()()}",
        "<([{}])>",
        "[<>({}){}[([])<>]]",
        "(((((((((())))))))))",
      )

      val corruptedLines = Table(
        "line",
        "(]",
        "{()()()>",
        "(((()))}",
        "<([]){()}[{}])",

        "{([(<{}[<>[]}>{[]{[(<()>",
        "[[<[([]))<([[{}[[()]]]",
        "[{[{({}]{}}([{[{{{}}([]",
        "[<(<(<(<{}))><([]([]()",
        "<{([([[(<>()){}]>(<<{{",
      )

      forAll(legalLines) { line =>
        assert(solution.parseLine(line) == Legal)
      }

      forAll(corruptedLines) { line =>
        assert(solution.parseLine(line).isInstanceOf[Corrupted])
      }

      assert(solution.totalSyntaxErrorScore(parseLines(exampleInput)) == 26397)
    }

    test("Part 1 input answer") {
      assert(solution.totalSyntaxErrorScore(parseLines(input)) == 316851)
    }

    test("Part 2 examples") {
      val lineExpectedeCompletions = Table(
        ("line", "expectedCompletion"),
        ("[({(<(())[]>[[{[]{<()<>>", "}}]])})]"),
        ("[(()[<>])]({[<{<<[]>>(", ")}>]})"),
        ("(((({<>}<{<{<>}{[]{[]{}", "}}>}>))))"),
        ("{<[[]]>}<{[{[{[]{()[[[]", "]]}}]}]}>"),
        ("<{([{{}}[<[[[<>{}]]]>[]]", "])}>"),
      )

      forAll(lineExpectedeCompletions) { (line, expectedCompletion) =>
        assert(solution.completeLine(line) == expectedCompletion)
      }

      assert(solution.middleCompletionScore(parseLines(exampleInput)) == 288957)
    }

    test("Part 2 input answer") {
      assert(solution.middleCompletionScore(parseLines(input)) == 2182912364L)
    }
  }

  class ParserCombinatorSolutionTest extends SolutionTest(ParserCombinatorSolution)

  class StackSolutionTest extends SolutionTest(StackSolution)

  class RecursiveDescentSolutionTest extends SolutionTest(RecursiveDescentSolution)
}
