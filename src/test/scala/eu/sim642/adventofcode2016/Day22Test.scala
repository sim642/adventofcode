package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day22._

class Day22Test extends FunSuite {

  val exampleInput =
    """Filesystem            Size  Used  Avail  Use%
      |/dev/grid/node-x0-y0   10T    8T     2T   80%
      |/dev/grid/node-x0-y1   11T    6T     5T   54%
      |/dev/grid/node-x0-y2   32T   28T     4T   87%
      |/dev/grid/node-x1-y0    9T    7T     2T   77%
      |/dev/grid/node-x1-y1    8T    0T     8T    0%
      |/dev/grid/node-x1-y2   11T    7T     4T   63%
      |/dev/grid/node-x2-y0   10T    6T     4T   60%
      |/dev/grid/node-x2-y1    9T    8T     1T   88%
      |/dev/grid/node-x2-y2    9T    6T     3T   66%""".stripMargin

  test("Part 1 input answer") {
    assert(countViablePairs(input) == 941)
  }

  test("Part 2 examples") {
    assert(stepsToGoal(exampleInput) == 7)
  }

  test("Part 2 input answer") {
    assert(stepsToGoal(input) == 249)
  }
}
