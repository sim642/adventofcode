package eu.sim642.adventofcode2016

import Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  test("parseRoom") {
    assert(parseRoom("aaaaa-bbb-z-y-x-123[abxyz]") == Room("aaaaa-bbb-z-y-x", 123, "abxyz"))
  }

  test("Part 1 examples") {
    assert(isReal(parseRoom("aaaaa-bbb-z-y-x-123[abxyz]")))
    assert(isReal(parseRoom("a-b-c-d-e-f-g-h-987[abcde]")))
    assert(isReal(parseRoom("not-a-real-room-404[oarel]")))
    assert(!isReal(parseRoom("totally-real-room-200[decoy]")))

    assert(realSectorIdSum("""aaaaa-bbb-z-y-x-123[abxyz]
                                    |a-b-c-d-e-f-g-h-987[abcde]
                                    |not-a-real-room-404[oarel]
                                    |totally-real-room-200[decoy]""".stripMargin) == 1514)
  }

  test("Part 1 input answer") {
    assert(realSectorIdSum(input) == 409147)
  }

  test("Part 2 examples") {
    assert(decrypt("qzmt-zixmtkozy-ivhz", 343) == "very encrypted name")
  }

  test("Part 2 input answer") {
    assert(northPoleObjectsSectorId(input) == 991)
  }
}
