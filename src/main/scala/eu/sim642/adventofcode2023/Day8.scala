package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.IterableImplicits.*
import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

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

  //def followInstructionsStepsGhost(input: Input): Int = {
  //  val Input(instructions, network) = input
  //  val startNodes = network.keys.filter(_.endsWith("A")).toSeq
  //  instructions.cycle.scanLeft(startNodes)({ (nodes, instruction) =>
  //    nodes.map({ node => (node, instruction) match {
  //      case (node, 'L') => network(node)._1
  //      case (node, 'R') => network(node)._2
  //    }})
  //  }).indexWhere(_.forall(_.endsWith("Z")))
  //}

  def followInstructionsStepsGhost(input: Input): Long = {
    val Input(instructions, network) = input

    def helper(startNode: Node): Int = {
      //var nodes = instructions.zipWithIndex.cycle.scanLeft((instructions.size - 1, startNode))({
      //  case ((i, node), ('L', j)) => j -> network(node)._1
      //  case ((i, node), ('R', j)) => j -> network(node)._2
      //})
      //println(NaiveCycleFinder.find(nodes).get)
      val nodes = instructions.cycle.scanLeft(startNode)({
        case (node, 'L') => network(node)._1
        case (node, 'R') => network(node)._2
      })
      val x = nodes.indexWhere(_.endsWith("Z"))
      println(s"$startNode -> $x")
      x
      //???
    }

    val startNodes = network.keys.filter(_.endsWith("A")).toSeq
    println(startNodes)
    val steps = startNodes.map(helper)
    NumberTheory.lcm(steps.map(_.toLong))
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
    println(followInstructionsStepsGhost(parseInput(input)))

    // part 2: 1457348313 - too low (Int overflowed in lcm)
  }
}
