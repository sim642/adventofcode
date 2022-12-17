package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder

import scala.math.Integral.Implicits.*

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

  def newShapePos(stopped: Set[Pos]): Pos = {
    Pos(2, stopped.view.map(_.y).maxOption.getOrElse(-1) + 4)
  }

  case class State(jetI: Int, nextShapeI: Int, currentShape: Set[Pos], stopped: Set[Pos], stoppedCount: Int = 0)(jets: Seq[Jet]) {
    def next: State = {
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
        State(newJetI, nextShapeI, currentShapeDown, stopped, stoppedCount)(jets)
      }
      else { // cannot move
        val newStopped = stopped ++ currentShapeJet2
        val newStopped2 = newStopped
        val newOffset = newShapePos(newStopped2)
        State(newJetI, (nextShapeI + 1) % shapes.size, shapes(nextShapeI).map(_ + newOffset), newStopped2, stoppedCount + 1)(jets)
      }
    }

    def height: Int = stopped.view.map(_.y).max + 1

    def pruneTop(amount: Int): State = {
      val yBound = stopped.view.map(_.y).maxOption.getOrElse(amount - 1) - amount
      val pos = Pos(0, yBound + 1)
      val newStopped = stopped.filter(_.y > yBound).map(_ - pos)
      val newShape = currentShape.map(_ - pos)
      copy(currentShape = newShape, stopped = newStopped, stoppedCount = -1)(jets)
    }
  }

  object State {
    def apply(jets: Seq[Jet]): State = {
      val newOffset = newShapePos(Set.empty)
      State(0, 1, shapes.head.map(_ + newOffset), Set.empty)(jets)
    }
  }

  trait Part {
    val defaultFinalStoppedCount: Long

    def towerHeight(jets: Seq[Jet], finalStoppedCount: Long = defaultFinalStoppedCount): Long
  }

  object Part1 extends Part {
    override val defaultFinalStoppedCount: Long = 2022

    override def towerHeight(jets: Seq[Jet], finalStoppedCount: Long): Long = {
      val initialState = State(jets)
      val finalState = Iterator.iterate(initialState)(_.next).find(_.stoppedCount == finalStoppedCount).get
      finalState.height
    }
  }

  object Part2 extends Part {
    override val defaultFinalStoppedCount: Long = 1000000000000L

    override def towerHeight(jets: Seq[Jet], finalStoppedCount: Long): Long = {
      val initialState = State(jets)
      val cycle = NaiveCycleFinder.findBy(initialState, _.next)(_.pruneTop(1))

      val states = Vector.iterate(initialState, cycle.stemCycleLength + 1)(_.next)
      val stemState = states(cycle.stemLength)
      val stemCycleState = states(cycle.stemCycleLength)

      val stemCount = stemState.stoppedCount
      val cycleCount = stemCycleState.stoppedCount - stemCount
      val (cycleRepeat, tailCount) = (finalStoppedCount - stemCount) /% cycleCount
      val tailState = states.view.find(_.stoppedCount == stemCount + tailCount.toInt).get

      val stemHeight = stemState.height
      val cycleHeight = stemCycleState.height - stemHeight
      val tailHeight = tailState.height - stemHeight
      stemHeight + cycleRepeat * cycleHeight + tailHeight
    }
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
    println(Part1.towerHeight(parseJets(input)))
    println(Part2.towerHeight(parseJets(input)))
  }
}
