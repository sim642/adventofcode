package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day14._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day14Test extends FunSuite with ScalaCheckPropertyChecks {

  val comet = Reindeer(14, 10, 127)
  val dancer = Reindeer(16, 11, 162)
  val exampleReindeers = Seq(comet, dancer)

  test("Part 1 examples") {

    val timeDists = Table(
      ("time", "cometDist", "dancerDist"),
      (1, 14, 16),
      (10, 140, 160),
      (11, 140, 176),
      (1000, 1120, 1056),
    )

    forAll (timeDists) { (time, cometDist, dancerDist) =>
      assert(comet.distanceAfter(time) == cometDist)
      assert(dancer.distanceAfter(time) == dancerDist)
    }


    assert(winningDistance(exampleReindeers, 1000) == 1120)
  }

  test("Part 1 input answer") {
    assert(winningDistance(input) == 2696)
  }

  test("Part 2 examples") {
    assert(winningPoints(exampleReindeers, 1000) == 689)
  }

  test("Part 2 input answer") {
    assert(winningPoints(input) == 1084)
  }
}
