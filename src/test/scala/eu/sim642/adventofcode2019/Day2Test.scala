package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day2._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day2Test extends FunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val initialExpectedFinals = Table(
      ("initial", "expectedFinal"),
      (Vector(1,0,0,0,99), Vector(2,0,0,0,99)),
      (Vector(2,3,0,3,99), Vector(2,3,0,6,99)),
      (Vector(2,4,4,5,99,0), Vector(2,4,4,5,99,9801)),
      (Vector(1,1,1,4,99,5,6,0,99), Vector(30,1,1,4,2,5,6,0,99)),

      (Vector(1,9,10,3,2,3,11,0,99,30,40,50), Vector(3500,9,10,70,2,3,11,0,99,30,40,50)),
    )

    forAll (initialExpectedFinals) { (initial, expectedFinal) =>
      assert(Program(initial).execFinal.code == expectedFinal)
    }
  }

  test("Part 1 input answer") {
    assert(execPosition0(parseCode(input)) == 2782414)
  }

  test("Part 2 input answer") {
    assert(findNounVerb(parseCode(input)) == 9820)
  }
}
