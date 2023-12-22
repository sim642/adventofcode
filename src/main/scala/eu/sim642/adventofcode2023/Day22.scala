package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.box.Box3
import eu.sim642.adventofcodelib.pos.Pos3
import eu.sim642.adventofcodelib.IteratorImplicits._

import scala.annotation.tailrec

object Day22 {

  type Brick = Box3

  def fallBrick(brick: Brick, yOffset: Int): Brick = {
    val offset = Pos3(0, 0, -yOffset)
    Box3(brick.min + offset, brick.max + offset)
  }

  def settle(bricks: Seq[Brick]): Seq[Brick] = {
    val zBricks = bricks.sortBy(_.min.z)

    zBricks.foldLeft(List.empty[Brick])({ (acc, brick) =>
      val yOffset = Iterator.from(1).find({ yOffset =>
        val newBrick = fallBrick(brick, yOffset)
        newBrick.min.z == 0 || acc.exists(accBrick => (newBrick intersect accBrick).isDefined)
      }).get - 1

      val newBrick = fallBrick(brick, yOffset)
      newBrick :: acc // put new brick first to make exists faster?
    })
  }

  def computeSupportedBy(bricks: Seq[Brick]): Map[Brick, Set[Brick]] = {
    bricks
      .map({ brick =>
        val newBrick = fallBrick(brick, 1)
        val supportedBy = bricks.view.filter(supportBrick =>
          supportBrick != brick && (supportBrick intersect newBrick).isDefined
        ).toSet
        brick -> supportedBy
      })
      .toMap
  }

  def countDisintegrable(bricks: Seq[Brick]): Int = {
    val settledBricks = settle(bricks)
    val supportedBy = computeSupportedBy(settledBricks)

    val cannotRemove = supportedBy.values
      .filter(_.size == 1)
      .map(_.head)
    bricks.size - cannotRemove.toSet.size
  }

  def sumDisintegrateFall(bricks: Seq[Brick]): Int = {
    val settledBricks = settle(bricks)
    val supportedBy = computeSupportedBy(settledBricks)
    val supports = (for {
      (brick, brickSupportedBy) <- supportedBy.iterator
      supportBrick <- brickSupportedBy
    } yield supportBrick -> brick).groupMapReduce(_._1)(p => Set(p._2))(_ ++ _).withDefaultValue(Set.empty)

    // non-standard BFS that uses visited for neighbors
    @tailrec
    def countDisintegrateFall(todo: Set[Brick], visited: Set[Brick]): Int = {
      val newVisited = visited ++ todo
      val newTodo = for {
        brick <- todo
        supportedBrick <- supports(brick)
        if supportedBy(supportedBrick).subsetOf(newVisited) // all supports visited
      } yield supportedBrick
      if (newTodo.isEmpty)
        newVisited.size - 1 // exclude removed brick
      else
        countDisintegrateFall(newTodo, newVisited)
    }

    settledBricks.map(brick => countDisintegrateFall(Set(brick), Set.empty)).sum
  }


  def parseBrick(s: String): Brick = s match {
    case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
      val p1 = Pos3(x1.toInt, y1.toInt, z1.toInt)
      val p2 = Pos3(x2.toInt, y2.toInt, z2.toInt)
      Box3(p1, p2)
  }

  def parseBricks(input: String): Seq[Brick] = input.linesIterator.map(parseBrick).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countDisintegrable(parseBricks(input)))
    println(sumDisintegrateFall(parseBricks(input)))

    // part 1: 963 - too high (cannotRemove checked against unsettled)
    // part 1: 482 - too high (self-supporting bricksanswer for someone else)

    // part 2: 34751 - too low (re-settling may fall into same place)
  }
}
