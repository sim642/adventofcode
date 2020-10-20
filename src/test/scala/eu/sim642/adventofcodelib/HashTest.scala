package eu.sim642.adventofcodelib

import Hash._
import org.scalatest.funsuite.AnyFunSuite

class HashTest extends AnyFunSuite {

  test("md5") {
    assert(md5("The quick brown fox jumps over the lazy dog") == "9e107d9d372bb6826bd81d3542a419d6")
  }
}
