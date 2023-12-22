package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.box.Box3
import eu.sim642.adventofcodelib.pos.Pos3

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

  def countDisintegrable(bricks: Seq[Brick]): Int = {
    val settledBricks = settle(bricks)
    println(settledBricks)

    //def countSupports(brick: Brick): Int = {
    //  val newBrick = fallBrick(brick, -1)
    //  bricks.count(b => (b intersect newBrick).isDefined)
    //}
    //
    //settledBricks.count(countSupports(_) <= 1)

    val cannotRemove = settledBricks
      .map({ brick =>
        val newBrick = fallBrick(brick, 1)
        val supportedBy = settledBricks.filter(b => b != brick && (b intersect newBrick).isDefined)
        println(s"$brick supported by $supportedBy")
        supportedBy
      })
      .filter(_.size == 1)
      .map(_.head)
    println(bricks.size)
    println(cannotRemove.toSet.size)
    bricks.size - cannotRemove.toSet.size
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

    // part 1: 963 - too high (cannotRemove checked against unsettled)
    // part 1: 482 - too high (self-supporting bricksanswer for someone else)
  }
}
