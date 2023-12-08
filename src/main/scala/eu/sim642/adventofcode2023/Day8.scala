package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IterableImplicits._

object Day8 {

  type Node = String
  type Network = Map[Node, (Node, Node)]

  case class Input(instructions: Seq[Char], network: Network)

  def followInstructionsSteps(input: Input): Int = {
    val Input(instructions, network) = input
    instructions.cycle.scanLeft("AAA")({
      case (node, 'L') => network(node)._1
      case (node, 'R') => network(node)._2
    }).indexOf("ZZZ")
  }


  def parseNode(s: String): (Node, (Node, Node)) = s match {
    case s"$node = ($left, $right)" => node -> (left, right)
  }

  def parseInput(input: String): Input = input.linesIterator.toSeq match {
    case instructions +: _ +: network =>
      Input(instructions, network.map(parseNode).toMap)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(followInstructionsSteps(parseInput(input)))
  }
}
