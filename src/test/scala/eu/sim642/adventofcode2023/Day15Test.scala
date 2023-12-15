package eu.sim642.adventofcode2023

import Day15._
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {

  private val exampleInput =
    """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"""

  test("HASH") {
    assert(hash("HASH") == 52)
  }

  test("Part 1 examples") {
    assert(sumStepHashes(parseSteps(exampleInput)) == 1320)
  }

  test("Part 1 input answer") {
    assert(sumStepHashes(parseSteps(input)) == 507666)
  }
}
