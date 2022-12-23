package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcodelib.IterableImplicits.*
import eu.sim642.adventofcodelib.SeqImplicits.*
import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

object Day23 {

  private val initialOffsetOrder = Seq(
    Pos(0, -1), // north
    Pos(0, 1), // south
    Pos(-1, 0), // west
    Pos(1, 0), // east
  )

  def simulateRound(elves: Set[Pos], offsetOrder: Seq[Pos]): (Set[Pos], Seq[Pos]) = {

    val proposed = elves.view.map({ pos =>
      if (Pos.allOffsets.forall(offset => !elves(pos + offset)))
        pos -> pos
      else {
        val validOffset = offsetOrder.find({ offset =>
          val offsetLeft = offset + offset.left
          val offsetRight = offset + offset.right
          val offsets = Seq(offset, offsetLeft, offsetRight)
          offsets.forall(offset => !elves(pos + offset))
        })

        validOffset match {
          case None => pos -> pos
          case Some(offset) => pos -> (pos + offset)
        }
      }
    }).toMap

    val proposedInverse = proposed.groupCount(_._2)

    val newElves = proposed.view.map({ case (elf, newElf) =>
      if (proposedInverse(newElf) == 1)
        newElf
      else
        elf
    }).toSet

    //println(newElves)
    val newOffsetOrder = offsetOrder.rotateLeft(1)

    (newElves, newOffsetOrder)
  }

  def countEmpty(elves: Set[Pos]): Int = {
    val box = Box.bounding(elves)
    box.size[Int] - elves.size
  }

  def simulateEmpty(initialElves: Set[Pos], rounds: Int = 10): Int = {
    val (finalElves, _) = Iterator.iterate((initialElves, initialOffsetOrder))(simulateRound)(rounds)
    countEmpty(finalElves)
  }

  def countRounds(initialElves: Set[Pos]): Int = {
    val cycle = NaiveCycleFinder.findBy((initialElves, initialOffsetOrder), simulateRound)(_._1)
    assert(cycle.cycleLength == 1) // should be fixpoint
    cycle.stemCycleLength
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseElves(input: String): Set[Pos] = {
    (for {
      (row, y) <- parseGrid(input).view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '#'
    } yield Pos(x, y)).toSet
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(simulateEmpty(parseElves(input)))
    println(countRounds(parseElves(input)))
  }
}
