package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.LazyListImplicits._

import scala.annotation.tailrec

object Day17 {

  private val shapes = Seq(
    Set(Pos(0, 0), Pos(1, 0), Pos(2, 0), Pos(3, 0)),
    Set(Pos(1, 0), Pos(0, 1), Pos(1, 1), Pos(2, 1), Pos(1, 2)),
    Set(Pos(0, 0), Pos(1, 0), Pos(2, 0), Pos(2, 1), Pos(2, 2)),
    Set(Pos(0, 0), Pos(0, 1), Pos(0, 2), Pos(0, 3)),
    Set(Pos(0, 0), Pos(1, 0), Pos(0, 1), Pos(1, 1)),
  )

  enum Jet(val offset: Pos) {
    case Left extends Jet(Pos(-1, 0))
    case Right extends Jet(Pos(1, 0))
  }

  def towerHeight(jets: Seq[Jet]): Int = {

    def newShapePos(stopped: Set[Pos]): Pos = {
      Pos(2, stopped.view.map(_.y).maxOption.getOrElse(-1) + 4)
    }

    @tailrec
    def helper(jets: LazyList[Jet], shapes: LazyList[Set[Pos]], currentShape: Set[Pos], stopped: Set[Pos], stoppedCount: Int = 0): Set[Pos] = {
      //println(stoppedCount)
      //printStopped(stopped)
      if (stoppedCount < 2022) {
        val jet #:: newJets = jets: @unchecked
        val currentShapeJet = currentShape.map(_ + jet.offset)
        val currentShapeJet2 =
          if (currentShapeJet.forall(p => p.x >= 0 && p.x < 7 && !stopped.contains(p))) // can move
            currentShapeJet
          else // cannot move
            currentShape
        val currentShapeDown = currentShapeJet2.map(_ + Pos(0, -1))
        if (currentShapeDown.forall(p => p.y >= 0 && !stopped.contains(p))) { // can move
          helper(newJets, shapes, currentShapeDown, stopped, stoppedCount)
        }
        else { // cannot move
          val newStopped = stopped ++ currentShapeJet2
          val newOffset = newShapePos(newStopped)
          helper(newJets, shapes.tail, shapes.head.map(_ + newOffset), newStopped, stoppedCount + 1)
        }
      }
      else
        stopped
    }

    val newOffset = newShapePos(Set.empty)
    val finalStopped = helper(jets.to(LazyList).cycle, shapes.to(LazyList).cycle.tail, shapes.head.map(_ + newOffset), Set.empty)
    finalStopped.view.map(_.y).max + 1
  }

  def printStopped(stopped: Set[Pos]): Unit = {
    if (stopped.nonEmpty) {
      val Box(min, max) = Box.bounding(stopped)
      for (y <- (0 to max.y).reverse) {
        for (x <- 0 until 7)
          print(if (stopped.contains(Pos(x, y))) '#' else '.')
        println()
      }
    }
  }

  def parseJet(c: Char): Jet = c match {
    case '<' => Jet.Left
    case '>' => Jet.Right
  }

  def parseJets(input: String): Seq[Jet] = input.map(parseJet)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(towerHeight(parseJets(input)))
  }
}
