package eu.sim642.adventofcode2021

import Day10._
import Day10Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day10Test extends Suites(
  new ParserCombinatorSolutionTest,
  new StackSolutionTest,
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

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {
    import solution._

    test("Part 1 examples") {
      assert(parseLine("()") == Legal)
      assert(parseLine("[]") == Legal)
      assert(parseLine("([])") == Legal)
      assert(parseLine("{()()()}") == Legal)
      assert(parseLine("<([{}])>") == Legal)
      assert(parseLine("[<>({}){}[([])<>]]") == Legal)
      assert(parseLine("(((((((((())))))))))") == Legal)

      assert(parseLine("(]").isInstanceOf[Corrupted])
      assert(parseLine("{()()()>").isInstanceOf[Corrupted])
      assert(parseLine("(((()))}").isInstanceOf[Corrupted])
      assert(parseLine("<([]){()}[{}])").isInstanceOf[Corrupted])

      assert(parseLine("{([(<{}[<>[]}>{[]{[(<()>").isInstanceOf[Corrupted])
      assert(parseLine("[[<[([]))<([[{}[[()]]]").isInstanceOf[Corrupted])
      assert(parseLine("[{[{({}]{}}([{[{{{}}([]").isInstanceOf[Corrupted])
      assert(parseLine("[<(<(<(<{}))><([]([]()").isInstanceOf[Corrupted])
      assert(parseLine("<{([([[(<>()){}]>(<<{{").isInstanceOf[Corrupted])

      assert(totalSyntaxErrorScore(parseLines(exampleInput)) == 26397)
    }

    test("Part 1 input answer") {
      assert(totalSyntaxErrorScore(parseLines(input)) == 316851)
    }

    test("Part 2 examples") {
      assert(completeLine("[({(<(())[]>[[{[]{<()<>>") == "}}]])})]")
      assert(completeLine("[(()[<>])]({[<{<<[]>>(") == ")}>]})")
      assert(completeLine("(((({<>}<{<{<>}{[]{[]{}") == "}}>}>))))")
      assert(completeLine("{<[[]]>}<{[{[{[]{()[[[]") == "]]}}]}]}>")
      assert(completeLine("<{([{{}}[<[[[<>{}]]]>[]]") == "])}>")

      assert(completionScore("}}]])})]") == 288957)
      assert(completionScore(")}>]})") == 5566)
      assert(completionScore("}}>}>))))") == 1480781)
      assert(completionScore("]]}}]}]}>") == 995444)
      assert(completionScore("])}>") == 294)

      assert(middleCompletionScore(parseLines(exampleInput)) == 288957)
    }

    test("Part 2 input answer") {
      assert(middleCompletionScore(parseLines(input)) == 2182912364L)
    }
  }

  class ParserCombinatorSolutionTest extends SolutionTest(ParserCombinatorSolution)
  
  class StackSolutionTest extends SolutionTest(StackSolution)
}
