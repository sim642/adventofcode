package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day20._

class Day20Test extends FunSuite {

  val exampleInput1 = "^WNE$"
  val exampleInput2 = "^N(E|W)N$"
  val exampleInput3 = "^ENWWW(NEEE|SSE(EE|N))$"
  val exampleInput4 = "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$"
  val exampleInput5 = "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
  val exampleInput6 = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

  test("parseInput") {
    println(parseInput(exampleInput1))
    println(parseInput(exampleInput2))
    println(parseInput(exampleInput3))
    println(parseInput(exampleInput4))
    println(parseInput(exampleInput5))
    println(parseInput(exampleInput6))
  }

  test("Part 1 examples") {
    assert(furthestRoom(exampleInput1) == 3)
    assert(furthestRoom(exampleInput3) == 10)
    assert(furthestRoom(exampleInput4) == 18)
    assert(furthestRoom(exampleInput5) == 23)
    assert(furthestRoom(exampleInput6) == 31)
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
