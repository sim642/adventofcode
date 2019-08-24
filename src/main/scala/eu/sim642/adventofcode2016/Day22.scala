package eu.sim642.adventofcode2016

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.graph.{AStar, BFS, GraphSearch, Heuristic, UnitNeighbors}

object Day22 {

  case class Disk(used: Int, available: Int) {
    def size: Int = used + available
  }

  def countViablePairs(nodes: Map[Pos, Disk]): Int = {
    val viablePairs = for {
      a@(aPos, aDisk) <- nodes
      if aDisk.used > 0
      b@(bPos, bDisk) <- nodes
      if bPos != aPos
      if aDisk.used <= bDisk.available
    } yield (a, b)

    viablePairs.size
  }

  def printNodes(nodes: Map[Pos, Disk]): Unit = {
    val maxX = nodes.keys.map(_.x).max
    val maxY = nodes.keys.map(_.y).max
    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        val disk = nodes(Pos(x, y))
        print(f"${disk.used}%2d/${disk.used + disk.available}%2d ")
      }
      println()
    }
  }

  sealed trait DiskType {
    val gridChar: Char
  }
  case object Empty extends DiskType {
    override val gridChar: Char = '_'
  }
  case object Normal extends DiskType {
    override val gridChar: Char = '.'
  }
  case object Oversize extends DiskType {
    override val gridChar: Char = '#'
  }

  private val originPos = Pos.zero

  def nodesToTypes(nodes: Map[Pos, Disk]): Map[Pos, DiskType] = {
    val oversizeLimit = nodes(originPos).size
    nodes.view.mapValues({ disk =>
      if (disk.used == 0)
        Empty
      else if (disk.used > oversizeLimit)
        Oversize
      else
        Normal
    }).toMap
  }

  def printNodeTypes(nodeTypes: Map[Pos, DiskType]): Unit = {
    val maxX = nodeTypes.keys.map(_.x).max
    val maxY = nodeTypes.keys.map(_.y).max
    for (y <- 0 to maxY) {
      for (x <- 0 to maxX) {
        val diskType = nodeTypes(Pos(x, y))
        print(f"${diskType.gridChar} ")
      }
      println()
    }
  }

  def stepsToGoal(nodes: Map[Pos, Disk]): Int = {
    val nodeTypes = nodesToTypes(nodes)

    case class NodesState(holePos: Pos, goalDataPos: Pos) {
      lazy val moves: Seq[NodesState] = {
        for {
          offset <- Pos.axisOffsets
          newHolePos = holePos + offset
          if nodeTypes.contains(newHolePos)
          if nodeTypes(newHolePos) != Oversize
          newGoalDataPos = if (newHolePos == goalDataPos) holePos else goalDataPos
        } yield NodesState(newHolePos, newGoalDataPos)
      }
    }

    val graphSearch = new GraphSearch[NodesState] with UnitNeighbors[NodesState] with Heuristic[NodesState] {
      override val startNode: NodesState = {
        val holePos = nodeTypes.find(_._2 == Empty).get._1
        val goalDataPos = nodeTypes.keys.filter(_.y == 0).maxBy(_.x)
        NodesState(holePos, goalDataPos)
      }

      override def unitNeighbors(nodesState: NodesState): IterableOnce[NodesState] = nodesState.moves

      override def isTargetNode(nodesState: NodesState, dist: Int): Boolean = {
        nodesState.goalDataPos == originPos
      }

      override def heuristic(nodesState: NodesState): Int = {
        val holeToGoal = nodesState.holePos manhattanDistance nodesState.goalDataPos
        val goalToOrigin = nodesState.goalDataPos manhattanDistance originPos
        holeToGoal + 5 * goalToOrigin // 5 moves for hole to go around goal
      }
    }

    AStar.search(graphSearch).target.get._2
  }

  private val nodeRegex = """/dev/grid/node-x(\d+)-y(\d+)\s+(\d+)T\s+(\d+)T\s+(\d+)T\s+(\d+)%""".r

  val parseNode: PartialFunction[String, (Pos, Disk)] = {
    case nodeRegex(x, y, _, used, avail, _) =>
      Pos(x.toInt, y.toInt) -> Disk(used.toInt, avail.toInt)
  }

  def parseNodes(input: String): Map[Pos, Disk] = input.linesIterator.collect(parseNode).toMap

  def countViablePairs(input: String): Int = countViablePairs(parseNodes(input))

  def stepsToGoal(input: String): Int = stepsToGoal(parseNodes(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countViablePairs(input))
    println(stepsToGoal(input))
  }
}
