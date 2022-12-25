package eu.sim642.adventofcode2022

import Day21.*
import Day21Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends Suites(
  new Part1Test,
  new InvertPart2SolutionTest,
  new BinarySearchPart2SolutionTest,
  new LinearPart2SolutionTest,
  new DerivativePart2SolutionTest,
)

object Day21Test {

  val exampleInput =
    """root: pppw + sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 5
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32""".stripMargin

  class Part1Test extends AnyFunSuite {

    test("Part 1 examples") {
      assert(evalRoot(parseMonkeys(exampleInput)) == 152)
    }

    test("Part 1 input answer") {
      assert(evalRoot(parseMonkeys(input)) == 158731561459602L)
    }
  }

  abstract class Part2SolutionTest(part2Solution: Part2Solution) extends AnyFunSuite {

    test("Part 2 examples") {
      assert(part2Solution.findHumn(parseMonkeys(exampleInput)) == 301)
    }

    test("Part 2 input answer") {
      assert(part2Solution.findHumn(parseMonkeys(input)) == 3769668716709L)
    }
  }

  class InvertPart2SolutionTest extends Part2SolutionTest(InvertPart2Solution)

  class BinarySearchPart2SolutionTest extends Part2SolutionTest(BinarySearchPart2Solution)

  class LinearPart2SolutionTest extends Part2SolutionTest(LinearPart2Solution)

  class DerivativePart2SolutionTest extends Part2SolutionTest(DerivativePart2Solution)
}
