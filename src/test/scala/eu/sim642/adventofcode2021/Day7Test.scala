package eu.sim642.adventofcode2021

import Day7._
import Day7Test._
import eu.sim642.adventofcode2019.intcode
import org.scalacheck.Gen
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.Configuration
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day7Test extends Suites(
  new BaseTest,
  new LinearSolutionTest,
  new BinarySolutionTest,
  new MathSolutionTest,
)

object Day7Test {

  val exampleInput = "16,1,2,0,4,2,7,1,2,14"

  class BaseTest extends AnyFunSuite {
    test("Part 1 examples") {
      val exampleCrabs = parseCrabs(exampleInput)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 2) == 37)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 1) == 41)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 3) == 39)
      assert(Part1Fuel.alignPosFuel(exampleCrabs, 10) == 71)
    }

    test("Part 2 examples") {
      val exampleCrabs = parseCrabs(exampleInput)
      assert(Part2Fuel.alignPosFuel(exampleCrabs, 5) == 168)
      assert(Part2Fuel.alignPosFuel(exampleCrabs, 2) == 206)
    }

    test("Input intcode easter egg") {
      // https://www.reddit.com/r/adventofcode/comments/raysm8/2021_day_7_out_of_curiosity_i_looked_at_my_input/
      val outputStr = intcode.ProgramState(intcode.parseProgram(input)).outputs.map(_.toChar).mkString
      assert(outputStr == "Ceci n'est pas une intcode program\n")
    }
  }

  sealed abstract class SolutionTest(solution: Solution) extends AnyFunSuite {
    test("Part 1 examples") {
      assert(solution.Part1.minAlignPosFuel(parseCrabs(exampleInput)) == 37)
    }

    test("Part 1 input answer") {
      assert(solution.Part1.minAlignPosFuel(parseCrabs(input)) == 336721)
    }

    test("Part 2 examples") {
      assert(solution.Part2.minAlignPosFuel(parseCrabs(exampleInput)) == 168)
    }

    test("Part 2 input answer") {
      assert(solution.Part2.minAlignPosFuel(parseCrabs(input)) == 91638945)
    }
  }

  trait EquivalentTest(solution: Solution) extends AnyFunSuite with ScalaCheckPropertyChecks with Configuration {
    implicit override val generatorDrivenConfig = PropertyCheckConfiguration(minSuccessful = 100) // default is 10

    val crabsGen = Gen.nonEmptyListOf(Gen.choose(0, 1000)) // TODO: fix solutions for negative arguments

    test("Part 1 equivalent") {
      forAll(crabsGen) { (crabs: Seq[Int]) =>
        assert(solution.Part1.minAlignPosFuel(crabs) == LinearSolution.Part1.minAlignPosFuel(crabs))
      }
    }

    test("Part 2 equivalent") {
      forAll(crabsGen) { (crabs: Seq[Int]) =>
        assert(solution.Part2.minAlignPosFuel(crabs) == LinearSolution.Part2.minAlignPosFuel(crabs))
      }
    }
  }

  class LinearSolutionTest extends SolutionTest(LinearSolution)

  class BinarySolutionTest extends SolutionTest(BinarySolution) with EquivalentTest(BinarySolution)

  class MathSolutionTest extends SolutionTest(MathSolution) with EquivalentTest(MathSolution)
}
