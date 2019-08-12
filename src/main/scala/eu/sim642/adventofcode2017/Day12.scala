package eu.sim642.adventofcode2017

import eu.sim642.adventofcodelib.graph.{BFS, GraphComponents, GraphTraversal, UnitNeighbors}

object Day12 {

  type Node = Int
  type NodeNeighbors = Map[Node, Seq[Node]]
  type NodeComponent = Set[Node]

  private val nodeRegex = """(\d+) <-> (\d+(?:, \d+)*)""".r

  def parseNode(nodeInput: String): (Node, Seq[Node]) = nodeInput match {
    case nodeRegex(nodeStr, neighborsStr) =>
      nodeStr.toInt -> neighborsStr.split(", ").toSeq.map(_.toInt)
  }

  def parseNodes(nodesInput: String): NodeNeighbors = nodesInput.linesIterator.map(parseNode).toMap

  def bfs(nodeNeighbors: NodeNeighbors, start: Node): NodeComponent = {

    val graphTraversal = new GraphTraversal[Node] with UnitNeighbors[Node] {
      override val startNode: Node = start

      override def unitNeighbors(node: Node): TraversableOnce[Node] = nodeNeighbors(node)
    }

    BFS.traverse(graphTraversal).nodes
  }

  def groupSize(input: String, startNode: Int = 0): Int = bfs(parseNodes(input), startNode).size

  def bfsGroups(nodeNeighbors: NodeNeighbors): Set[NodeComponent] = {

    val graphComponents = new GraphComponents[Node] {
      override def nodes: TraversableOnce[Node] = nodeNeighbors.keySet

      override def unitNeighbors(node: Node): TraversableOnce[Node] = nodeNeighbors(node)
    }

    BFS.components(graphComponents)
  }

  def groupCount(input: String): Int = bfsGroups(parseNodes(input)).size

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(groupSize(input))
    println(groupCount(input))
  }
}
