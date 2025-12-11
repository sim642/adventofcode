package eu.sim642.adventofcode2025

import Day11.*
import Day11Test.*
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends Suites(
  new ViaMapSolutionTest,
)

object Day11Test {

  val exampleInput =
    """aaa: you hhh
      |you: bbb ccc
      |bbb: ddd eee
      |ccc: ddd eee fff
      |ddd: ggg
      |eee: out
      |fff: out
      |ggg: out
      |hhh: ccc fff iii
      |iii: out""".stripMargin

  val exampleInput2 =
    """svr: aaa bbb
      |aaa: fft
      |fft: ccc
      |bbb: tty
      |tty: ccc
      |ccc: ddd eee
      |ddd: hub
      |hub: fff
      |eee: dac
      |dac: fff
      |fff: ggg hhh
      |ggg: out
      |hhh: out""".stripMargin

  abstract class SolutionTest(solution: Solution) extends AnyFunSuite {
    test("Part 1 examples") {
      assert(solution.Part1.countPaths(parseDevices(exampleInput)) == 5)
    }

    test("Part 1 input answer") {
      assert(solution.Part1.countPaths(parseDevices(input)) == 643)
    }

    test("Part 2 examples") {
      assert(solution.Part2.countPaths(parseDevices(exampleInput2)) == 2)
    }

    test("Part 2 input answer") {
      assert(solution.Part2.countPaths(parseDevices(input)) == 417190406827152L)
    }
  }

  class ViaMapSolutionTest extends SolutionTest(ViaMapSolution)
}
