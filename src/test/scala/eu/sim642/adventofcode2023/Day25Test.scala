package eu.sim642.adventofcode2023

import Day25._
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {

  private val exampleInput =
    """jqt: rhn xhk nvd
      |rsh: frs pzl lsr
      |xhk: hfx
      |cmg: qnr nvd lhk bvb
      |rhn: xhk bvb hfx
      |bvb: xhk hfx
      |pzl: lsr hfx nvd
      |qnr: nvd
      |ntq: jqt hfx bvb xhk
      |nvd: lhk
      |lsr: lhk
      |rzs: qnr cmg lsr rsh
      |frs: qnr lhk lsr""".stripMargin

  test("Part 1 examples") {
    assert(disconnectComponents(parseEdges(exampleInput)) == 54)
  }

  test("Part 1 input answer") {
    assert(disconnectComponents(parseEdges(input)) == 538560)
  }
}
