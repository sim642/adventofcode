package eu.sim642.adventofcode2021

import Day6.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day6Test extends AnyFunSuite with ScalaCheckPropertyChecks {

  val exampleInput = "3,4,3,1,2"

  test("Part 1 example states") {
    val states = Table(
      "state",
      Seq(3,4,3,1,2),
      Seq(2,3,2,0,1),
      Seq(1,2,1,6,0,8),
      Seq(0,1,0,5,6,7,8),
      Seq(6,0,6,4,5,6,7,8,8),
      Seq(5,6,5,3,4,5,6,7,7,8),
      Seq(4,5,4,2,3,4,5,6,6,7),
      Seq(3,4,3,1,2,3,4,5,5,6),
      Seq(2,3,2,0,1,2,3,4,4,5),
      Seq(1,2,1,6,0,1,2,3,3,4,8),
      Seq(0,1,0,5,6,0,1,2,2,3,7,8),
      Seq(6,0,6,4,5,6,0,1,1,2,6,7,8,8,8),
      Seq(5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8),
      Seq(4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8),
      Seq(3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8),
      Seq(2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7),
      Seq(1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8),
      Seq(0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8),
      Seq(6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8),
    )

    val it = Iterator.iterate(input2state(states.head))(stepState)
    forAll(states) { state =>
      assert(it.next() == input2state(state))
    }
  }

  test("Part 1 examples") {
    assert(countLanternfish(parseInput(exampleInput), 18) == 26)
    assert(countLanternfish(parseInput(exampleInput)) == 5934)
  }

  test("Part 1 input answer") {
    assert(countLanternfish(parseInput(input)) == 346063)
  }
}
