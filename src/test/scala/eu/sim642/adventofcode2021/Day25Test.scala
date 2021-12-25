package eu.sim642.adventofcode2021

import Day25._
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {

  val exampleInput2 =
    """v...>>.vv>
      |.vv>>.vv..
      |>>.>v>...v
      |>>v>>.>.v.
      |v>v.vv.v..
      |>.>>..v...
      |.vv..>.>v.
      |v.v..>>v.v
      |....v..v.>""".stripMargin

  test("Part 1 examples") {
    assert(step(parseInput("...>>>>>...")) == parseInput("...>>>>.>.."))
    assert(step(parseInput("...>>>>.>..")) == parseInput("...>>>.>.>."))

    assert(step(parseInput(
      """..........
        |.>v....v..
        |.......>..
        |..........""".stripMargin
    )) == parseInput(
      """..........
        |.>........
        |..v....v>.
        |..........""".stripMargin
    ))

    assert(findStoppedStep(parseInput(exampleInput2)) == 58)
  }

  test("Part 1 input answer") {
    assert(findStoppedStep(parseInput(input)) == 305)
  }
}
