package eu.sim642.adventofcode2017

import Day18._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import scala.collection.immutable.Queue
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite with ScalaCheckPropertyChecks {

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

  test("Part 1 example states") {
    val states = Table(
      ("pc", "registers", "snds"),
      (0, Map(), Queue()),
      (1, Map('a' -> 1L), Queue()),
      (2, Map('a' -> 3L), Queue()),
      (3, Map('a' -> 9L), Queue()),
      (4, Map('a' -> 4L), Queue()),
      (5, Map('a' -> 4L), Queue(4L)),
      (6, Map('a' -> 0L), Queue(4L)),
      (7, Map('a' -> 0L), Queue(4L)),
      (8, Map('a' -> 0L), Queue(4L)),
      (9, Map('a' -> 1L), Queue(4L)),
      (7, Map('a' -> 1L), Queue(4L)),
      (6, Map('a' -> 1L), Queue(4L))
    )

    val it = Part1.iterateSmallStep(parseInstructions(exampleInput1))
    forAll (states) { (pc, registers, snds) =>
      val state = it.next()
      assert(state.pc == pc)
      assert(state.registers == registers)
      assert(state.snds == snds)
    }
  }

  test("Part 1 example") {
    assert(Part1.firstRcv(exampleInput1) == 4L)
  }

  test("Part 1 input answer") {
    assert(Part1.firstRcv(input) == 8600L)
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

    val it = Part2.iterateSmallStepPair(parseInstructions(exampleInput2))
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
    assert(Part2.countSnd1(exampleInput2) == 3)
  }

  test("Part 2 input answer") {
    assert(Part2.countSnd1(input) == 7239)
  }
}
