package eu.sim642.adventofcode2017

import Day3._
import eu.sim642.AdventOfCodeSuite
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day3Test extends FunSuite with PropertyChecks with AdventOfCodeSuite {

  test("squarePos") {
    val coords = Table(
      ("square", "pos"),
      (1,       Pos(0, 0)),

      (2,       Pos(1, 0)),
      (3,       Pos(1, 1)),
      (4,       Pos(0, 1)),
      (5,       Pos(-1, 1)),
      (6,       Pos(-1, 0)),
      (7,       Pos(-1, -1)),
      (8,       Pos(0, -1)),
      (9,       Pos(1, -1)),

      (10,      Pos(2, -1)),
      (11,      Pos(2, 0)),
      (12,      Pos(2, 1)),
      (13,      Pos(2, 2)),
      (14,      Pos(1, 2)),
      (15,      Pos(0, 2)),
      (16,      Pos(-1, 2)),
      (17,      Pos(-2, 2)),
      (18,      Pos(-2, 1)),
      (19,      Pos(-2, 0)),
      (20,      Pos(-2, -1)),
      (21,      Pos(-2, -2)),
      (22,      Pos(-1, -2)),
      (23,      Pos(0, -2)),
      (24,      Pos(1, -2)),
      (25,      Pos(2, -2))
    )

    forAll (coords) { (square, pos) =>
      assert(squarePos(square) == pos)
    }
  }

  test("Part 1 example") {
    assert(steps(1) == 0)
    assert(steps(12) == 3)
    assert(steps(23) == 2)
    assert(steps(1024) == 31)
  }

  test("Part 1 input answer") {
    assert(steps(input) == 419)
  }

  test("SpiralSumIterator") {
    val sums = Table("sum", 1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147, 304, 330, 351, 362, 747, 806)
    val it = new SpiralSumIterator
    forAll (sums) { sum =>
      assert(it.next() == sum)
    }
  }

  test("spiralSumLarger") {
    forAll (Gen.choose(0, 1000000)) { (input: Int) =>
      val sum = spiralSumLarger(input)

      assert(sum > input)

      val it = new SpiralSumIterator
      assert(it.contains(sum))
    }
  }

  test("Part 2 input answer") {
    assert(spiralSumLarger(input) == 295229)
  }
}
