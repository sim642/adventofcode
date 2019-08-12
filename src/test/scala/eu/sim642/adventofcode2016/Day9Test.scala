package eu.sim642.adventofcode2016

import Day9._
import org.scalatest.FunSuite

class Day9Test extends FunSuite {

  test("Part 1 examples") {
    assert(Part1.decompressedLength("ADVENT") == 6)
    assert(Part1.decompressedLength("A(1x5)BC") == 7)
    assert(Part1.decompressedLength("(3x3)XYZ") == 9)
    assert(Part1.decompressedLength("A(2x2)BCD(2x2)EFG") == 11)
    assert(Part1.decompressedLength("(6x1)(1x3)A") == 6)
    assert(Part1.decompressedLength("X(8x2)(3x3)ABCY") == 18)
  }

  test("Part 1 input answer") {
    assert(Part1.decompressedLength(input) == 97714)
  }

  test("Part 2 examples") {
    assert(Part2.decompressedLength("(3x3)XYZ") == 9)
    assert(Part2.decompressedLength("X(8x2)(3x3)ABCY") == "XABCABCABCABCABCABCY".length)
    assert(Part2.decompressedLength("(27x12)(20x12)(13x14)(7x10)(1x12)A") == 241920)
    assert(Part2.decompressedLength("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN") == 445)
  }

  test("Part 2 input answer") {
    assert(Part2.decompressedLength(input) == 10762972461L)
  }
}
