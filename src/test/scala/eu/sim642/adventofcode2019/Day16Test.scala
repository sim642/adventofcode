package eu.sim642.adventofcode2019

import Day16._
import org.scalatest.FunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day16Test extends FunSuite with ScalaCheckPropertyChecks {

  test("Part 1 examples (small)") {
    val input = "12345678"

    val phasesExpectedEight = Table(
      ("phases", "expectedEight"),
      (1, "48226158"),
      (2, "34040438"),
      (3, "03415518"),
      (4, "01029498"),
    )

    forAll (phasesExpectedEight) { (phases, expectedEight) =>
      assert(stepPhasesEight(parseSignal(input), phases) == expectedEight)
    }
  }

  test("Part 1 examples (large)") {
    val inputExpectedEight = Table(
      ("input", "expectedEight"),
      ("80871224585914546619083218645595", "24176176"),
      ("19617804207202209144916044189917", "73745418"),
      ("69317163492948606335995924319873", "52432133"),
    )

    forAll (inputExpectedEight) { (input, expectedEight) =>
      assert(stepPhasesEight(parseSignal(input)) == expectedEight)
    }
  }

  test("Part 1 input answer") {
    assert(stepPhasesEight(parseSignal(input)) == "25131128")
  }
}
