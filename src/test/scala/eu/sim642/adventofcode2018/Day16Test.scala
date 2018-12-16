package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day16._

class Day16Test extends FunSuite {

  val exampleSample =
    """Before: [3, 2, 1, 1]
      |9 2 1 2
      |After:  [3, 2, 2, 1]""".stripMargin

  test("Part 1 examples") {
    assert(countSampleOpcodes(parseSample(exampleSample)) == 3)
  }

  test("Part 1 input answer") {
    assert(count3Samples(input) == 567)
  }

  test("Part 2 input answer") {
    assert(runProgram(input) == 610)
  }
}
