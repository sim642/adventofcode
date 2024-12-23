package eu.sim642.adventofcode2024

import Day23._
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {

  val exampleInput =
    """kh-tc
      |qp-kh
      |de-cg
      |ka-co
      |yn-aq
      |qp-ub
      |cg-tb
      |vc-aq
      |tb-ka
      |wh-tc
      |yn-cg
      |kh-ub
      |ta-co
      |de-co
      |tc-td
      |tb-wq
      |wh-td
      |ta-ka
      |td-qp
      |aq-cg
      |wq-ub
      |ub-vc
      |de-ta
      |wq-aq
      |wq-vc
      |wh-yn
      |ka-de
      |kh-ta
      |co-tc
      |wh-qp
      |tb-vc
      |td-yn""".stripMargin

  test("Part 1 examples") {
    assert(count3CliquesT(parseEdges(exampleInput)) == 7)
  }

  test("Part 1 input answer") {
    assert(count3CliquesT(parseEdges(input)) == 1437)
  }

  test("Part 2 examples") {
    assert(lanPartyPassword(parseEdges(exampleInput)) == "co,de,ka,ta")
  }

  test("Part 2 input answer") {
    assert(lanPartyPassword(parseEdges(input)) == "da,do,gx,ly,mb,ns,nt,pz,sc,si,tp,ul,vl")
  }
}
