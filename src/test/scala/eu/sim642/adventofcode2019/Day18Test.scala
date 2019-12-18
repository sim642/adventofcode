package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day18._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day18Test extends FunSuite with ScalaCheckPropertyChecks {

  val exampleInput =
    """#########
      |#b.A.@.a#
      |#########""".stripMargin

  val exampleInput2 =
    """########################
      |#f.D.E.e.C.b.A.@.a.B.c.#
      |######################.#
      |#d.....................#
      |########################""".stripMargin

  val exampleInput3 =
    """########################
      |#...............b.C.D.f#
      |#.######################
      |#.....@.a.B.c.d.A.e.F.g#
      |########################""".stripMargin

  val exampleInput4 =
    """#################
      |#i.G..c...e..H.p#
      |########.########
      |#j.A..b...f..D.o#
      |########@########
      |#k.E..a...g..B.n#
      |########.########
      |#l.F..d...h..C.m#
      |#################""".stripMargin

  val exampleInput5 =
    """########################
      |#@..............ac.GI.b#
      |###d#e#f################
      |###A#B#C################
      |###g#h#i################
      |########################""".stripMargin

  test("Part 1 examples") {
    val inputExpectedSteps = Table(
      ("input", "expectedSteps"),
      (exampleInput, 8),
      (exampleInput2, 86),
      (exampleInput3, 132),
      (exampleInput4, 136),
      (exampleInput5, 81),
    )

    forAll (inputExpectedSteps) { (input, expectedSteps) =>
      assert(collectKeysSteps(parseInput(input)) == expectedSteps)
    }
  }

  test("Part 1 input answer") {
    assert(collectKeysSteps(parseInput(input)) == 4204)
  }
}
