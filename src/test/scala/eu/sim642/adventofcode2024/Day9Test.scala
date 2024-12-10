package eu.sim642.adventofcode2024

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  val exampleInput = "12345"
  val exampleInput2 = "2333133121414131402"

  test("Part 1 examples") {
    assert(filesystemToString(Part1.compact(parseFilesystem(exampleInput))) ==
      "022111222") // trailing Free trimmed
    assert(filesystemToString(Part1.compact(parseFilesystem(exampleInput2))) ==
      "0099811188827773336446555566") // trailing Free trimmed
    assert(Part1.compactChecksum(parseFilesystem(exampleInput2)) == 1928)

    assert(Part1.compactChecksum(parseFilesystem(exampleInput)) == 60) // not in text
  }

  test("Part 1 input answer") {
    assert(Part1.compactChecksum(parseFilesystem(input)) == 6310675819476L)
  }

  test("Part 2 examples") {
    assert(filesystemToString(Part2.compact(parseFilesystem(exampleInput2))) ==
      "00992111777.44.333....5555.6666.....8888..")
    assert(Part2.compactChecksum(parseFilesystem(exampleInput2)) == 2858)
  }

  test("Part 2 input answer") {
    assert(Part2.compactChecksum(parseFilesystem(input)) == 6335972980679L)
  }
}
