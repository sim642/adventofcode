package eu.sim642.adventofcode2021

import Day25._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import eu.sim642.adventofcodelib.IteratorImplicits._

class Day25Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput1 =
    """...>...
      |.......
      |......>
      |v.....>
      |......>
      |.......
      |..vvv..""".stripMargin

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

    val expecteds = Table(
      "expected",
      """..vv>..
        |.......
        |>......
        |v.....>
        |>......
        |.......
        |....v..""".stripMargin,
      """....v>.
        |..vv...
        |.>.....
        |......>
        |v>.....
        |.......
        |.......""".stripMargin,
      """......>
        |..v.v..
        |..>v...
        |>......
        |..>....
        |v......
        |.......""".stripMargin,
      """>......
        |..v....
        |..>.v..
        |.>.v...
        |...>...
        |.......
        |v......""".stripMargin
    )

    val it = Iterator.iterate(parseInput(exampleInput1))(step).tail
    forAll(expecteds) { expected =>
      assert(it.next() == parseInput(expected))
    }

    assert(findStoppedStep(parseInput(exampleInput2)) == 58)
  }

  test("Part 1 input answer") {
    assert(findStoppedStep(parseInput(input)) == 305)
  }
}
