package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day5._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class Day5Test extends FunSuite with ScalaCheckPropertyChecks {

  test("Part 1 input answer") {
    assert(execDiagnostic(parseProgram(input), 1) == 15259545)
  }

  test("Part 2 examples") {
    val programInputsOutputs = Table(
      ("program", "inputs", "expectedOutputs"),
      // equal to 8, position mode
      (Vector(3,9,8,9,10,9,4,9,99,-1,8), LazyList(7), LazyList(0)),
      (Vector(3,9,8,9,10,9,4,9,99,-1,8), LazyList(8), LazyList(1)),
      (Vector(3,9,8,9,10,9,4,9,99,-1,8), LazyList(9), LazyList(0)),
      // less than 8, position mode
      (Vector(3,9,7,9,10,9,4,9,99,-1,8), LazyList(7), LazyList(1)),
      (Vector(3,9,7,9,10,9,4,9,99,-1,8), LazyList(8), LazyList(0)),
      (Vector(3,9,7,9,10,9,4,9,99,-1,8), LazyList(9), LazyList(0)),
      // equal to 8, immediate mode
      (Vector(3,3,1108,-1,8,3,4,3,99), LazyList(7), LazyList(0)),
      (Vector(3,3,1108,-1,8,3,4,3,99), LazyList(8), LazyList(1)),
      (Vector(3,3,1108,-1,8,3,4,3,99), LazyList(9), LazyList(0)),
      // less than 8, immediate mode
      (Vector(3,3,1107,-1,8,3,4,3,99), LazyList(7), LazyList(1)),
      (Vector(3,3,1107,-1,8,3,4,3,99), LazyList(8), LazyList(0)),
      (Vector(3,3,1107,-1,8,3,4,3,99), LazyList(9), LazyList(0)),

      // is non-zero, position mode
      (Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), LazyList(-1), LazyList(1)),
      (Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), LazyList(0), LazyList(0)),
      (Vector(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), LazyList(1), LazyList(1)),
      // is non-zero, immediate mode
      (Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), LazyList(-1), LazyList(1)),
      (Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), LazyList(0), LazyList(0)),
      (Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), LazyList(1), LazyList(1)),

      // compare to 8
      (Vector(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), LazyList(7), LazyList(999)),
      (Vector(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), LazyList(8), LazyList(1000)),
      (Vector(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), LazyList(9), LazyList(1001)),
    )

    forAll (programInputsOutputs) { (program, inputs, expectedOutputs) =>
      val outputs = execInputs(program, inputs)
      assert(outputs == expectedOutputs)
    }
  }

  test("Part 2 input answer") {
    assert(execDiagnostic(parseProgram(input), 5) == 7616021)
  }
}
