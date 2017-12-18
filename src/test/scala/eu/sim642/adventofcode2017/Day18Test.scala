package eu.sim642.adventofcode2017

import Day18._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

class Day18Test extends FunSuite with PropertyChecks {

  val exampleInput = """set a 1
                       |add a 2
                       |mul a a
                       |mod a 5
                       |snd a
                       |set a 0
                       |rcv a
                       |jgz a -1
                       |set a 1
                       |jgz a -2""".stripMargin

  test("parseInstructions") {
    assert(parseInstructions(exampleInput) == Vector(
      Set('a', ConstValue(1)),
      Add('a', ConstValue(2)),
      Mul('a', RegisterValue('a')),
      Mod('a', ConstValue(5)),
      Snd(RegisterValue('a')),
      Set('a', ConstValue(0)),
      Rcv('a'),
      Jgz(RegisterValue('a'), ConstValue(-1)),
      Set('a', ConstValue(1)),
      Jgz(RegisterValue('a'), ConstValue(-2))
    ))
  }

  test("Part 1 example states") {
    val states = Table(
      ("pc", "registers", "lastSnd"),
      (0, Map(), None),
      (1, Map('a' -> 1L), None),
      (2, Map('a' -> 3L), None),
      (3, Map('a' -> 9L), None),
      (4, Map('a' -> 4L), None),
      (5, Map('a' -> 4L), Some(4L)),
      (6, Map('a' -> 0L), Some(4L)),
      (7, Map('a' -> 0L), Some(4L)),
      (8, Map('a' -> 0L), Some(4L)),
      (9, Map('a' -> 1L), Some(4L)),
      (7, Map('a' -> 1L), Some(4L)),
      (6, Map('a' -> 1L), Some(4L))
    )

    val it = iterateSmallStep(parseInstructions(exampleInput))
    forAll (states) { (pc, registers, lastSnd) =>
      val state = it.next()
      assert(state.pc == pc)
      assert(state.registers == registers)
      assert(state.lastSnd == lastSnd)
    }
  }

  test("Part 1 example") {
    assert(firstRcv(exampleInput) == 4L)
  }

  test("Part 1 input answer") {
    assert(firstRcv(input) == 8600L)
  }
}
