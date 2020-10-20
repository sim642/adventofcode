package eu.sim642.adventofcode2019

import Day2._
import intcode._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val initialExpectedFinals = Table(
      ("initial", "expectedFinal"),
      ("1,0,0,0,99", "2,0,0,0,99"),
      ("2,3,0,3,99", "2,3,0,6,99"),
      ("2,4,4,5,99,0", "2,4,4,5,99,9801"),
      ("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99"),

      ("1,9,10,3,2,3,11,0,99,30,40,50", "3500,9,10,70,2,3,11,0,99,30,40,50"),
    )

    forAll (initialExpectedFinals) { (initial, expectedFinal) =>
      assert(ProgramState(parseProgram(initial)).execFinal.memory == parseProgram(expectedFinal))
    }
  }

  test("Part 1 input answer") {
    assert(execNounVerb(parseProgram(input)) == 2782414)
  }

  test("Part 2 input answer") {
    assert(findNounVerb(parseProgram(input)) == 9820)
  }
}
