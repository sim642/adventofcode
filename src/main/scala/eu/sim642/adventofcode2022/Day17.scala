package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.IteratorImplicits.IndexIteratorOps

import scala.annotation.tailrec
import scala.math.Integral.Implicits.*

object Day17 {

  def parseShape(s: String): Set[Pos] = {
    (for {
      (row, y) <- s.linesIterator.toList.reverseIterator.zipWithIndex
      (cell, x) <- row.iterator.zipWithIndex
      if cell == '#'
    } yield Pos(x, y)).toSet
  }

  private val shapesStr =
    """####
      |
      |.#.
      |###
      |.#.
      |
      |..#
      |..#
      |###
      |
      |#
      |#
      |#
      |#
      |
      |##
      |##""".stripMargin

  private val shapes = shapesStr.split("\n\n").map(parseShape).toVector

  enum Jet(val offset: Pos) {
    case Left extends Jet(Pos(-1, 0))
    case Right extends Jet(Pos(1, 0))
  }

  /**
   * Number of top rows to keep during simulation, lower ones are removed.
   * Constant determined after the fact to still get correct answers.
   */
  private val keepTopRows = 40

  case class State(jetI: Int, shapeI: Int, stopped: Set[Pos] = Set.empty, maxY: Int = -1)(jets: Seq[Jet]) {

    def next: State = {

      @tailrec
      def helper(jetI: Int, currentShape: Set[Pos]): State = {
        val jet = jets(jetI)
        val newJetI = (jetI + 1) % jets.size
        val currentShapeJet = currentShape.map(_ + jet.offset)
        val currentShapeJet2 =
          if (currentShapeJet.forall(p => p.x >= 0 && p.x < 7 && !stopped.contains(p))) // can move
            currentShapeJet
          else // cannot move
            currentShape
        val currentShapeDown = currentShapeJet2.map(_ + Pos(0, -1))
        if (currentShapeDown.forall(p => p.y >= 0 && !stopped.contains(p))) // can move
          helper(newJetI, currentShapeDown)
        else { // cannot move
          val newStopped = stopped ++ currentShapeJet2
          val newMaxY = maxY max currentShapeJet2.view.map(_.y).max
          val newStopped2 = newStopped.filter(_.y >= newMaxY - keepTopRows) // optimize by keeping stopped set small
          State(newJetI, (shapeI + 1) % shapes.size, newStopped2, newMaxY)(jets)
        }
      }

      val newOffset = Pos(2, maxY + 4)
      helper(jetI, shapes(shapeI).map(_ + newOffset))
    }

    def height: Int = maxY + 1

    def cycleInvariant: State = {
      val pos = Pos(0, maxY - keepTopRows)
      val newStopped = stopped.map(_ - pos)
      copy(stopped = newStopped, maxY = -1)(jets)
    }
  }

  object State {
    def apply(jets: Seq[Jet]): State = State(0, 0)(jets)
  }

  trait Part {
    val defaultFinalStoppedCount: Long

    def towerHeight(jets: Seq[Jet], finalStoppedCount: Long = defaultFinalStoppedCount): Long
  }

  object Part1 extends Part {
    override val defaultFinalStoppedCount: Long = 2022

    override def towerHeight(jets: Seq[Jet], finalStoppedCount: Long): Long = {
      val initialState = State(jets)
      val finalState = Iterator.iterate(initialState)(_.next)(finalStoppedCount.toInt)
      finalState.height
    }
  }

  object Part2 extends Part {
    override val defaultFinalStoppedCount: Long = 1000000000000L

    override def towerHeight(jets: Seq[Jet], finalStoppedCount: Long): Long = {
      val initialState = State(jets)
      val cycle = NaiveCycleFinder.findBy(initialState, _.next)(_.cycleInvariant) // TODO: findBy returns Indexing

      val (cycleRepeat, tailLength) = (finalStoppedCount - cycle.stemLength) /% cycle.cycleLength
      val tailState = Iterator.iterate(cycle.cycleHead)(_.next)(tailLength.toInt)

      val stemHeight = cycle.cycleHead.height
      val cycleHeight = cycle.cycleHeadRepeat.height - stemHeight
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
