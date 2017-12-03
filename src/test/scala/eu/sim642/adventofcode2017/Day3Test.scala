package eu.sim642.adventofcode2017

import Day3._
import org.scalacheck.Gen
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day3Test extends FunSuite with PropertyChecks {

  test("coord") {
    val coords = Table(
      ("square", "expectedCoord"),
      (1,       (0, 0)),

      (2,       (1, 0)),
      (3,       (1, 1)),
      (4,       (0, 1)),
      (5,       (-1, 1)),
      (6,       (-1, 0)),
      (7,       (-1, -1)),
      (8,       (0, -1)),
      (9,       (1, -1)),

      (10,      (2, -1)),
      (11,      (2, 0)),
      (12,      (2, 1)),
      (13,      (2, 2)),
      (14,      (1, 2)),
      (15,      (0, 2)),
      (16,      (-1, 2)),
      (17,      (-2, 2)),
      (18,      (-2, 1)),
      (19,      (-2, 0)),
      (20,      (-2, -1)),
      (21,      (-2, -2)),
      (22,      (-1, -2)),
      (23,      (0, -2)),
      (24,      (1, -2)),
      (25,      (2, -2))
    )

    forAll (coords) { (square, expectedCoord) =>
      assert(coord(square) == expectedCoord)
    }
  }

  test("Part 1 example") {
    assert(distance(1) == 0)
    assert(distance(12) == 3)
    assert(distance(23) == 2)
    assert(distance(1024) == 31)
  }

  test("Part 1 input answer") {
    assert(distance(input) == 419)
  }

  test("SumIterator") {
    val sums = Table("sum", 1, 1, 2, 4, 5, 10, 11, 23, 25, 26, 54, 57, 59, 122, 133, 142, 147, 304, 330, 351, 362, 747, 806)
    val it = new SumIterator
    forAll (sums) { sum =>
      assert(it.next() == sum)
    }
  }

  test("sumLarger") {
    forAll (Gen.choose(0, 1000000)) { (input: Int) =>
      println(input)
      val sum = sumLarger(input)

      assert(sum > input)

      val it = new SumIterator
      assert(it.contains(sum))
    }
  }

  test("Part 2 input answer") {
    assert(sumLarger(input) == 295229)
  }
}
