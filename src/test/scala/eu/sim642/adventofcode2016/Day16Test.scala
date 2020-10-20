package eu.sim642.adventofcode2016

import Day16._
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(checksum("110010110100") == "100")

    assert(fillChecksum("10000", 20) == "01100")
  }

  test("Part 1 input answer") {
    assert(fillChecksum(input, part1Length) == "10010100110011100")
  }

  test("Part 2 input answer") {
    assert(fillChecksum(input, part2Length) == "01100100101101100")
  }
}
