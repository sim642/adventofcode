package eu.sim642.adventofcode2016

import eu.sim642.adventofcodelib.Hash
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.graph._

object Day17 {

  private val startPos = Pos.zero
  private val targetPos = Pos(3, 3)
  private val vaultBox = Box(startPos, targetPos)

  private val moveOffsets = Seq( // in order
    'U' -> Pos(0, -1),
    'D' -> Pos(0, 1),
    'L' -> Pos(-1, 0),
    'R' -> Pos(1, 0),
  )

  case class VaultPos(pos: Pos, path: String)(md5: Hash.Digest) {
    private lazy val doors = {
      md5("").take(4).map(_ >= 'b')
    }

    def moves: Seq[VaultPos] = {
      if (pos == targetPos) // "paths always end the first time they reach the bottom-right room"
        Seq.empty
      else {
        for {
          ((move, offset), isOpen) <- moveOffsets.zip(doors)
          if isOpen
          newPos = pos + offset
          if vaultBox.contains(newPos)
        } yield copy(pos = newPos, path = path + move)(md5.prefix(move.toString))
      }
    }
  }

  object VaultPos {
    def apply(passcode: String, pos: Pos, path: String): VaultPos = {
      VaultPos(pos, path)(Hash.md5.prefix(passcode + path))
    }
  }

  def shortestVaultPath(passcode: String): String = {

    val graphSearch = new GraphSearch[VaultPos] with UnitNeighbors[VaultPos] with Heuristic[VaultPos] {
      override val startNode: VaultPos = VaultPos(passcode, startPos, "")

      override def unitNeighbors(vaultPos: VaultPos): IterableOnce[VaultPos] = vaultPos.moves

      override def isTargetNode(vaultPos: VaultPos, dist: Int): Boolean = vaultPos.pos == targetPos

      override def heuristic(vaultPos: VaultPos): Int = vaultPos.pos manhattanDistance targetPos
    }

    AStar.search(graphSearch).target.get._1.path
  }

  def longestVaultPathLength(passcode: String): Int = {

    val graphTraversal = new GraphTraversal[VaultPos] with UnitNeighbors[VaultPos] {
      override val startNode: VaultPos = VaultPos(passcode, startPos, "")

      override def unitNeighbors(vaultPos: VaultPos): IterableOnce[VaultPos] = vaultPos.moves
    }

    BFS.traverse(graphTraversal).distances
      .filter(_._1.pos == targetPos)
      .values.max
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim
  val input = "yjjvjgan"

  def main(args: Array[String]): Unit = {
    println(shortestVaultPath(input))
    println(longestVaultPathLength(input))
  }
}
