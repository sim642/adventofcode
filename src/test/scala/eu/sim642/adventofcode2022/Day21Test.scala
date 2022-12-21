package eu.sim642.adventofcode2022

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  val exampleInput =
    """root: pppw + sjmn
      |dbpl: 5
      |cczh: sllz + lgvd
      |zczc: 2
      |ptdq: humn - dvpt
      |dvpt: 3
      |lfqf: 4
      |humn: 5
      |ljgn: 2
      |sjmn: drzm * dbpl
      |sllz: 4
      |pppw: cczh / lfqf
      |lgvd: ljgn * ptdq
      |drzm: hmdt - zczc
      |hmdt: 32""".stripMargin

  test("Part 1 examples") {
    assert(evalRoot(parseMonkeys(exampleInput)) == 152)
  }

  test("Part 1 input answer") {
    assert(evalRoot(parseMonkeys(input)) == 158731561459602L)
  }
}
