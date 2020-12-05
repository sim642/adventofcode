package eu.sim642.adventofcode2020

import Day5._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day5Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val inputExpectedSeatExpectedSeatId = Table(
      ("input", "expectedSeat", "expectedSeatId"),
      ("FBFBBFFRLR", Seat(44, 5), 357),
      ("BFFFBBFRRR", Seat(70, 7), 567),
      ("FFFBBBFRRR", Seat(14, 7), 119),
      ("BBFFBBFRLL", Seat(102, 4), 820),
    )

    forAll(inputExpectedSeatExpectedSeatId) { (input, expectedSeat, expectedSeatId) =>
      val seat = parseSeat(input)
      assert(seat == expectedSeat)
      assert(seat.seatId == expectedSeatId)
    }
  }

  test("Part 1 input answer") {
    assert(highestSeatId(parseSeats(input)) == 828)
  }
}
