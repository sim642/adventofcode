package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IterableImplicits.*
import eu.sim642.adventofcodelib.NumberTheory

object Day8 {

  type Node = String
  type Network = Map[Node, (Node, Node)]

  case class Input(instructions: Seq[Char], network: Network)

  def iterateInstructions(input: Input, startNode: Node): Iterator[Node] = {
    val Input(instructions, network) = input
    instructions.cycle.scanLeft(startNode)({
      case (node, 'L') => network(node)._1
      case (node, 'R') => network(node)._2
    })
  }

  def followInstructionsSteps(input: Input): Int =
    iterateInstructions(input, "AAA").indexOf("ZZZ")

  def followInstructionsStepsGhost(input: Input): Long = {
    val startNodes = input.network.keys.filter(_.endsWith("A"))
    val steps = startNodes.map(iterateInstructions(input, _).indexWhere(_.endsWith("Z")))
    NumberTheory.lcm(steps.map(_.toLong).toSeq)
  }


  def parseNode(s: String): (Node, (Node, Node)) = s match {
    case s"$node = ($left, $right)" => node -> (left, right)
  }

  def parseInput(input: String): Input = input.linesIterator.toSeq match {
    case instructions +: _ +: network =>
      Input(instructions, network.map(parseNode).toMap)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(followInstructionsSteps(parseInput(input)))
    println(followInstructionsStepsGhost(parseInput(input)))

    // part 2: 1457348313 - too low (Int overflowed in lcm)
  }
}
