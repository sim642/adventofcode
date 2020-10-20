package eu.sim642.adventofcode2018

import Day20._
import org.scalatest.funsuite.AnyFunSuite

class Day20Test extends AnyFunSuite {

  val exampleInput1 = "^WNE$"
  val exampleInput2 = "^N(E|W)N$"
  val exampleInput3 = "^ENWWW(NEEE|SSE(EE|N))$"
  val exampleInput4 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
  val exampleInput5 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
  val exampleInput6 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

  test("parseInput") {
    parseInput(exampleInput1)
    parseInput(exampleInput2)
    parseInput(exampleInput3)
    parseInput(exampleInput4)
    parseInput(exampleInput5)
    parseInput(exampleInput6)
  }

  test("Part 1 examples") {
    assert(furthestRoom(exampleInput1) == 3)
    assert(furthestRoom(exampleInput3) == 10)
    assert(furthestRoom(exampleInput4) == 18)
    assert(furthestRoom(exampleInput5) == 23)
    assert(furthestRoom(exampleInput6) == 31)
  }

  test("Part 1 exponential choice") {
    assert(furthestRoom("^N(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)(NS|)$") == 2)
  }

  test("Part 1 non-first choice") {
    assert(furthestRoom("^N(NS|EEWW)(NS|)$") == 3)
  }

  test("Part 1 input answer") {
    assert(furthestRoom(input) == 3633)
  }

  test("Part 2 crafted examples") {
    assert(farRooms(exampleInput1, 2) == 2)
    assert(farRooms(exampleInput3, 5) == 11)
  }

  test("Part 2 input answer") {
    assert(farRooms(input) == 8756)
  }
}
