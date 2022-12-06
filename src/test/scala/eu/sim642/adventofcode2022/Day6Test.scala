package eu.sim642.adventofcode2022

import Day6.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day6Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples") {
    val datastreamIndex = Table(
      ("datastream", "index"),

      ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7),
      ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5),
      ("nppdvjthqldpwncqszvftbrmjlhg", 6),
      ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10),
      ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11),
    )

    forAll (datastreamIndex) { (datastream, expectedIndex) =>
      assert(startOfPacketIndex(datastream) == expectedIndex)
    }
  }

  test("Part 1 input answer") {
    assert(startOfPacketIndex(input) == 1848)
  }
}
