package eu.sim642.adventofcode2024

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

  val exampleInput = "12345"
  val exampleInput2 = "2333133121414131402"

  test("Part 1 examples") {
    //assert(defragmentChecksum(parseFilesystem(exampleInput)) == ???)
    //println(defragment(parseFilesystem(exampleInput)))
    //println(defragment(parseFilesystem(exampleInput2)))
    assert(defragmentChecksum(parseFilesystem(exampleInput2)) == 1928)
  }

  test("Part 1 input answer") {
    assert(defragmentChecksum(parseFilesystem(input)) == 6310675819476L)
  }
}
