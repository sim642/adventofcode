package eu.sim642.adventofcode2024

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  val exampleInput = "12345"
  val exampleInput2 = "2333133121414131402"

  test("Part 1 examples") {
    //assert(Part1.compactChecksum(parseFilesystem(exampleInput)) == ???)
    //println(Part1.compact(parseFilesystem(exampleInput)))
    //println(Part1.compact(parseFilesystem(exampleInput2)))
    assert(Part1.compactChecksum(parseFilesystem(exampleInput2)) == 1928)
  }

  test("Part 1 input answer") {
    assert(Part1.compactChecksum(parseFilesystem(input)) == 6310675819476L)
  }

  test("Part 2 examples") {
    //println(Part2.compact(parseFilesystem(exampleInput2)))
    assert(Part2.compactChecksum(parseFilesystem(exampleInput2)) == 2858)
  }

  test("Part 2 input answer") {
    assert(Part2.compactChecksum(parseFilesystem(input)) == 6335972980679L)
  }
}
