package eu.sim642.adventofcodelib

import org.scalatest.FunSuite
import Hash._

class HashTest extends FunSuite {

  test("md5") {
    assert(md5("The quick brown fox jumps over the lazy dog") == "9e107d9d372bb6826bd81d3542a419d6")
  }
}
