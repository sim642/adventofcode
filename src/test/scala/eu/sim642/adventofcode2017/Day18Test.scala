package eu.sim642.adventofcode2017

import Day18._
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks

import scala.collection.immutable.Queue

class Day18Test extends FunSuite with PropertyChecks {

  val exampleInput1 = """set a 1
                       |add a 2
                       |mul a a
                       |mod a 5
                       |snd a
                       |set a 0
                       |rcv a
                       |jgz a -1
                       |set a 1
                       |jgz a -2""".stripMargin

  val exampleInput2 = """snd 1
                        |snd 2
                        |snd p
                        |rcv a
                        |rcv b
                        |rcv c
                        |rcv d""".stripMargin

  test("parseInstructions") {
    assert(parseInstructions(exampleInput1) == Vector(
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

  ignore("Part 1 example states") {
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

    val it = iterateSmallStep(parseInstructions(exampleInput1))
    forAll (states) { (pc, registers, lastSnd) =>
      val state = it.next()
      assert(state.pc == pc)
      assert(state.registers == registers)
      assert(state.lastSnd == lastSnd)
    }
  }

  ignore("Part 1 example") {
    assert(firstRcv(exampleInput1) == 4L)
  }

  ignore("Part 1 input answer") {
    assert(firstRcv(input) == 8600L)
  }

  test("Part 2 example states") {
    val states = Table(
      ("pc0", "registers0", "rcvs0", "pc1", "registers1", "rcvs1"),
      (0, Map('p' -> 0L), Queue(), 0, Map('p' -> 1L), Queue()),
      (1, Map('p' -> 0L), Queue(1L), 1, Map('p' -> 1L), Queue(1L)),
      (2, Map('p' -> 0L), Queue(1L, 2L), 2, Map('p' -> 1L), Queue(1L, 2L)),
      (3, Map('p' -> 0L), Queue(1L, 2L, 1L), 3, Map('p' -> 1L), Queue(1L, 2L, 0L)),
      (4, Map('p' -> 0L, 'a' -> 1L), Queue(2L, 1L), 4, Map('p' -> 1L, 'a' -> 1L), Queue(2L, 0L)),
      (5, Map('p' -> 0L, 'a' -> 1L, 'b' -> 2L), Queue(1L), 5, Map('p' -> 1L, 'a' -> 1L, 'b' -> 2L), Queue(0L)),
      (6, Map('p' -> 0L, 'a' -> 1L, 'b' -> 2L, 'c' -> 1L), Queue(), 6, Map('p' -> 1L, 'a' -> 1L, 'b' -> 2L, 'c' -> 0L), Queue())
    )

    val it = iterateSmallStepPair(parseInstructions(exampleInput2))
    forAll (states) { (pc0, registers0, rcvs0, pc1, registers1, rcvs1) =>
      val (state0, state1) = it.next()
      assert(state0.pc == pc0)
      assert(state0.registers == registers0)
      assert(state0.rcvs == rcvs0)
      assert(state1.pc == pc1)
      assert(state1.registers == registers1)
      assert(state1.rcvs == rcvs1)
    }
  }

  test("Part 2 example") {
    assert(countSnd1(exampleInput2) == 3)
  }

  test("Part 2 input answer") {
    assert(countSnd1(input) == 7239)
  }
}
