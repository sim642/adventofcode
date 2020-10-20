package eu.sim642.adventofcode2015

import Day25._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val posIndexTable = Table(
      ("pos", "index"),
      (Pos(1, 1), 1),
      (Pos(1, 2), 2),
      (Pos(2, 1), 3),
      (Pos(1, 3), 4),
      (Pos(2, 2), 5),
      (Pos(3, 1), 6),
      (Pos(1, 4), 7),
      (Pos(2, 3), 8),
      (Pos(3, 2), 9),
      (Pos(4, 1), 10),
    )

    forAll (posIndexTable) { (pos, index) =>
      assert(posIndex(pos) == index)
    }

    assert(getCode(Pos(1, 1)) == 20151125)
    assert(getCode(Pos(1, 2)) == 31916031)
    assert(getCode(Pos(6, 6)) == 27995004)
  }

  test("Part 1 input answer") {
    assert(getCode(input) == 2650453)
  }
}
