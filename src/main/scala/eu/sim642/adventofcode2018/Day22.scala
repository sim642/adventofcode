package eu.sim642.adventofcode2018

import Day22.Tool._
import Day22.CaveType._
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.graph.{AStar, GraphSearch, Heuristic, TargetNode}

import scala.collection.mutable

object Day22 {

  enum Tool {
    case Torch, ClimbingGear, Neither
  }

  enum CaveType(val riskLevel: Int, val allowedTools: Set[Tool]) {
    case Rocky extends CaveType(0, Set(ClimbingGear, Torch))
    case Wet extends CaveType(1, Set(ClimbingGear, Neither))
    case Narrow extends CaveType(2, Set(Torch, Neither))
  }

  def calculateErosionLevel(depth: Int, target: Pos, max: Pos): Grid[Int] = {
    val erosionLevel = mutable.Seq.fill(max.y + 1, max.x + 1)(-1)
    for (y <- 0 to max.y) {
      for (x <- 0 to max.x) {
        val geologicIndex = (y, x) match {
          case (0, 0) => 0
          case (y, x) if x == target.x && y == target.y => 0
          case (0, x) => x * 16807
          case (y, 0) => y * 48271
          case (y, x) => erosionLevel(y)(x - 1) * erosionLevel(y - 1)(x)
        }

        erosionLevel(y)(x) = (geologicIndex + depth) % 20183
      }
    }
    erosionLevel.toVector.map(_.toVector)
  }

  def calculateCaveType(depth: Int, target: Pos, max: Pos): Grid[CaveType] = {
    val erosionLevel = calculateErosionLevel(depth, target, max)
    erosionLevel.mapGrid({ _ % 3 match {
      case 0 => Rocky
      case 1 => Wet
      case 2 => Narrow
    }})
  }

  def totalRiskLevel(depth: Int, target: Pos): Int = {
    val caveType = calculateCaveType(depth, target, target)
    val riskLevel: Grid[Int] = caveType.mapGrid(_.riskLevel)
    riskLevel.map(_.sum).sum
  }

  def totalRiskLevel(input: String): Int = {
    val (depth, target) = parseInput(input)
    totalRiskLevel(depth, target)
  }

  def fastestToTarget(depth: Int, target: Pos): Int = {
    val caveType = calculateCaveType(depth, target, Pos(target.x * 3, target.y * 3))

    type PosTool = (Pos, Tool)

    val moveDistance = 1
    val toolDistance = 7

    val graphSearch = new GraphSearch[PosTool] with TargetNode[PosTool] with Heuristic[PosTool] {
      override val startNode: PosTool = (Pos.zero, Torch)

      override def neighbors(posTool: PosTool): IterableOnce[(PosTool, Int)] = {
        val (pos, tool) = posTool

        val moveNeighbors = for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if caveType.containsPos(newPos)
          if caveType(newPos).allowedTools.contains(tool)
        } yield (newPos, tool) -> moveDistance

        val toolNeighbors = for {
          newTool <- caveType(pos).allowedTools - tool
        } yield (pos, newTool) -> toolDistance

        moveNeighbors ++ toolNeighbors
      }

      override val targetNode: PosTool = (target, Torch)

      override def heuristic(posTool: PosTool): Int = {
        (posTool._1 manhattanDistance targetNode._1) * moveDistance +
          (if (posTool._2 != targetNode._2) toolDistance else 0)
      }
    }

    AStar.search(graphSearch).target.get._2
  }

  def fastestToTarget(input: String): Int = {
    val (depth, target) = parseInput(input)
    fastestToTarget(depth, target)
  }

  private val inputRegex =
    """depth: (\d+)
      |target: (\d+),(\d+)""".stripMargin.r

  def parseInput(input: String): (Int, Pos) = input match {
    case inputRegex(depth, targetX, targetY) => (depth.toInt, Pos(targetX.toInt, targetY.toInt))
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalRiskLevel(input))
    println(fastestToTarget(input))

    // part 2: 977 - too high
  }
}
