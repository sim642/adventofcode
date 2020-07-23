package eu.sim642.adventofcode2019

package object intcode {

  type Value = Long
  type Address = Int


  def parseProgram(input: String): Memory = Memory.parse(input) // TODO: inline parseProgram?
}
