package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day5._

class Day5Test extends FunSuite {

  test("Part 1 input answer") {
    assert(execDiagnostic(parseProgram(input)) == 15259545)
  }
}
