package eu.sim642.adventofcode2024

import Day18.*
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {

  val exampleInput =
    """5,4
      |4,2
      |4,5
      |3,0
      |2,1
      |6,3
      |2,4
      |1,5
      |0,6
      |3,3
      |2,6
      |5,1
      |1,2
      |5,5
      |2,5
      |6,5
      |1,4
      |0,4
      |6,4
      |1,1
      |6,1
      |1,0
      |0,5
      |1,6
      |2,0""".stripMargin

  test("Part 1 examples") {
    assert(exitSteps(parseBytes(exampleInput), Pos(6, 6), 12) == 22)
  }

  test("Part 1 input answer") {
    assert(exitSteps(parseBytes(input)) == 304)
  }

  test("Part 2 examples") {
    assert(findBlockingByte(parseBytes(exampleInput), Pos(6, 6)) == "6,1")
  }

  ignore("Part 2 input answer") { // TODO: optimize (~5.7s)
    assert(findBlockingByte(parseBytes(input)) == "50,28")
  }
}
