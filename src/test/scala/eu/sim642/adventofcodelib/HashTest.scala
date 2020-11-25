package eu.sim642.adventofcodelib

import Hash._
import eu.sim642.adventofcode2015.Day4Test
import eu.sim642.adventofcode2016.{Day14Test, Day17Test, Day5Test}
import org.scalatest.Suites
import org.scalatest.funsuite.AnyFunSuite

class HashTest extends AnyFunSuite {

  test("md5") {
    assert(md5("The quick brown fox jumps over the lazy dog") == "9e107d9d372bb6826bd81d3542a419d6")
  }
}

class HashSpeedTest extends Suites(
  new Day4Test,
  new Day5Test,
  new Day14Test,
  new Day17Test,
)
