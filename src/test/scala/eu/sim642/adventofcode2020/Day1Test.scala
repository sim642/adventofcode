package eu.sim642.adventofcode2020

import Day1._
import eu.sim642.adventofcode2020.Day1Test._
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day1Test extends Suites(
  new NaiveSolutionTest,
  new SetContainsSolutionTest,
)

object Day1Test {

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {

    val exampleInput =
      """1721
        |979
        |366
        |299
        |675
        |1456""".stripMargin

    test("Part 1 examples") {
      assert(solution.entryProduct2020(parseEntries(exampleInput), 2) == 514579)
    }

    test("Part 1 input answer") {
      assert(solution.entryProduct2020(parseEntries(input), 2) == 713184)
    }

    test("Part 2 examples") {
      assert(solution.entryProduct2020(parseEntries(exampleInput), 3) == 241861950)
    }

    test("Part 2 input answer") {
      assert(solution.entryProduct2020(parseEntries(input), 3) == 261244452)
    }
  }

  class NaiveSolutionTest extends SolutionTest(NaiveSolution)

  class SetContainsSolutionTest extends SolutionTest(SetContainsSolution)
}
