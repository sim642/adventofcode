package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day9._

class Day9Test extends FunSuite {

  test("Part 1 examples") {
    assert(ProgramState(parseProgram("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")).outputs == LazyList(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99).map(_.toLong))
    assert(ProgramState(parseProgram("1102,34915192,34915192,7,4,7,99,0")).outputs == LazyList(1219070632396864L))
    assert(ProgramState(parseProgram("104,1125899906842624,99")).outputs == LazyList(1125899906842624L))
  }

  test("Part 1 input answer") {
    assert(boostKeyCode(parseProgram(input)) == 2789104029L)
  }
}
