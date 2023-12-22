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
    //println(settledBricks)

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
        //println(s"$brick supported by $supportedBy")
        supportedBy
      })
      .filter(_.size == 1)
      .map(_.head)
    //println(bricks.size)
    //println(cannotRemove.toSet.size)
    bricks.size - cannotRemove.toSet.size
  }

  def sumDisintegrateFall(bricks: Seq[Brick]): Int = {
    val settledBricks = settle(bricks)

    val supportedBy = settledBricks
      .map({ brick =>
        val newBrick = fallBrick(brick, 1)
        val supportedBy = settledBricks.filter(b => b != brick && (b intersect newBrick).isDefined)
        brick -> supportedBy
      })
      .toMap

    val supports = (for {
      (brick, s) <- supportedBy.toSeq
      brick2 <- s
    } yield brick2 -> brick).groupMap(_._1)(_._2)
    //println(supports)

    //val settledSet = settledBricks.toSet
    //def countDisintegrateFall(remove: Brick, removed: Set[Brick]): Int = {
    //  val after = settle((settledSet - remove).toSeq).toSet
    //  println(settledSet)
    //  println(after)
    //  val diff = settledSet -- after ++ (after -- settledSet)
    //  (diff.size - 1) / 2
    //}

    //val settledSet = settledBricks.toSet
    //def countDisintegrateFall(remove: Brick, removed: Set[Brick]): Set[Brick] = {
    //  if (removed(remove))
    //    removed
    //  else {
    //    val s = supports.getOrElse(remove, Seq.empty)
    //    val newRemoved = removed + remove
    //    val x = s.filter(b => (supportedBy(b).toSet -- newRemoved).size == 1)
    //    x.foldLeft(newRemoved)((acc, b) => countDisintegrateFall(b, acc))
    //  }
    //}

    def countDisintegrateFall(todo: Set[Brick], visited: Set[Brick]): Int = {
      if (todo.isEmpty)
        visited.size - 1
      else {
        val b = todo.head
        val newTodo = todo - b
        val s = supports.getOrElse(b, Set.empty)
        val newVisited = visited + b
        val moreTodo = s.filter(x => (supportedBy(x).toSet -- newVisited).isEmpty).toSet
        countDisintegrateFall(newTodo ++ moreTodo, newVisited)
      }
    }

    settledBricks.map({ p =>
      val f = countDisintegrateFall(Set(p), Set.empty)
      println((p, f))
      f
    }).sum
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
