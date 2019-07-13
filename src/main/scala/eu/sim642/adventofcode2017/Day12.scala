package eu.sim642.adventofcode2017

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

  def bfs(nodeNeighbors: NodeNeighbors, startNode: Node): NodeComponent = {

    def helper(visited: Set[Node], toVisit: Set[Node]): NodeComponent = {
      val neighbors = toVisit.flatMap(nodeNeighbors)
      val newVisited = visited ++ toVisit
      val newToVisit = neighbors -- visited
      if (newToVisit.isEmpty)
        newVisited
      else
        helper(newVisited, newToVisit)
    }

    helper(Set.empty, Set(startNode))
  }

  def groupSize(input: String, startNode: Int = 0): Int = bfs(parseNodes(input), startNode).size

  def bfsGroups(nodeNeighbors: NodeNeighbors): Set[NodeComponent] = {
    if (nodeNeighbors.isEmpty)
      Set.empty
    else {
      val (startNode, _) = nodeNeighbors.head // take any node
      val group = bfs(nodeNeighbors, startNode) // find its component
      val restNodeNeighbors =
        nodeNeighbors.view.filterKeys(!group(_)).mapValues(_.filterNot(group))  // remove component from graph (nodes and edges)
          .toMap // copy map for efficiency rather than chaining FilteredKeys & MappedValues - https://stackoverflow.com/a/14883167/854540
      bfsGroups(restNodeNeighbors) + group
    }
  }

  def groupCount(input: String): Int = bfsGroups(parseNodes(input)).size

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(groupSize(input))
    println(groupCount(input))
  }
}
