package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.LazyListImplicits.*
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

import scala.annotation.tailrec

import scala.math.Integral.Implicits._

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
  def towerHeight2(jets: Seq[Jet]): Long = {

    def newShapePos(stopped: Set[Pos]): Pos = {
      Pos(2, stopped.view.map(_.y).maxOption.getOrElse(-1) + 4)
    }

    case class State(jetI: Int, nextShapeI: Int, currentShape: Set[Pos], stopped: Set[Pos], stoppedCount: Int = 0) {
      def next: State = {
        //println(stoppedCount)
        //printStopped(stopped)
        //println()
        val jet = jets(jetI)
        val newJetI = (jetI + 1) % jets.size
        val currentShapeJet = currentShape.map(_ + jet.offset)
        val currentShapeJet2 =
          if (currentShapeJet.forall(p => p.x >= 0 && p.x < 7 && !stopped.contains(p))) // can move
            currentShapeJet
          else // cannot move
            currentShape
        val currentShapeDown = currentShapeJet2.map(_ + Pos(0, -1))
        if (currentShapeDown.forall(p => p.y >= 0 && !stopped.contains(p))) { // can move
          State(newJetI, nextShapeI, currentShapeDown, stopped, stoppedCount)
        }
        else { // cannot move
          val newStopped = stopped ++ currentShapeJet2
         /* val shapeBox = Box.bounding(newStopped)
          //val fullRowY = (shapeBox.min.y to shapeBox.max.y).find(y => (0 until 7).forall(x => newStopped.contains(Pos(x, y))))
          val fullRowY = (shapeBox.min.y to shapeBox.max.y).find(y => (0 until 7).forall(x => newStopped.contains(Pos(x, y))))
          val newStopped2 = fullRowY match {
            case None => newStopped
            case Some(fullRowY) =>
              println(s"pruned at $fullRowY")
              Console.flush()
              newStopped.filter(_.y > fullRowY).map(_ - Pos(0, fullRowY + 1))
          }*/
          val newStopped2 = newStopped
          val newOffset = newShapePos(newStopped2)
          State(newJetI, (nextShapeI + 1) % shapes.size, shapes(nextShapeI).map(_ + newOffset), newStopped2, stoppedCount + 1)
        }
      }

      def pruneTop: State = {
        //printStopped(stopped)
        val amount = 10
        val yBound = stopped.view.map(_.y).maxOption.getOrElse(amount - 1) - amount
        val pos = Pos(0, yBound + 1)
        val newStopped = stopped.filter(_.y > yBound).map(_ - pos)
        val newShape = currentShape.map(_ - pos)
        //printStopped(newStopped)
        //println()
        copy(currentShape = newShape, stopped = newStopped, stoppedCount = -1)
      }

      def height: Int = stopped.view.map(_.y).max + 1
    }

    val newOffset = newShapePos(Set.empty)
    val initialState = State(0, 1, shapes.head.map(_ + newOffset), Set.empty)
    val cycle = NaiveCycleFinder.findBy(initialState, _.next)(_.pruneTop)
    //println(cycle)

    val total = 1000000000000L
    //val total = 2022L
    val pref = Vector.iterate(initialState, cycle.stemCycleLength + 1)(_.next)
    val stemHeight = pref(cycle.stemLength).height
    val stemCount = pref(cycle.stemLength).stoppedCount
    val cycleHeight = pref(cycle.stemCycleLength).height - stemHeight
    val cycleCount = pref(cycle.stemCycleLength).stoppedCount - stemCount
    val (cycleRepeat, tailCount) = (total - stemCount) /% cycleCount
    val tailHeight = pref.view.find(_.stoppedCount == stemCount + tailCount.toInt).get.height - stemHeight
    println((stemHeight, cycleHeight, tailHeight))
    println((stemCount, cycleCount, tailCount))

    stemHeight + cycleRepeat * cycleHeight + tailHeight
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
