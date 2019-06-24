package eu.sim642.adventofcode2016

import Day9._
import org.scalatest.FunSuite

class Day9Test extends FunSuite {

  test("Part 1 examples") {
    assert(decompressedLength("ADVENT") == 6)
    assert(decompressedLength("A(1x5)BC") == 7)
    assert(decompressedLength("(3x3)XYZ") == 9)
    assert(decompressedLength("A(2x2)BCD(2x2)EFG") == 11)
    assert(decompressedLength("(6x1)(1x3)A") == 6)
    assert(decompressedLength("X(8x2)(3x3)ABCY") == 18)
  }

  test("Part 1 input answer") {
    assert(decompressedLength(input) == 97714)
  }
}
