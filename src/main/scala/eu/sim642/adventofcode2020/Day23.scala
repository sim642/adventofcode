package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.SeqImplicits._
import eu.sim642.adventofcodelib.IntegralImplicits._
import eu.sim642.adventofcodelib.LazyListImplicits._

object Day23 {

  type Cups = Vector[Int]

  case class State(next: Map[Int, Int], current: Int, max: Int) {

    def toCups(from: Int) = {
      from #:: LazyList.unfold0(from)(i => Some(next(i))).takeWhile(_ != from)
    }

    override def toString: String = toCups(current).toString
  }

  def simulateMove(state: State): State = {
    val State(next, current, max) = state
    val pick1 = next(current)
    val pick2 = next(pick1)
    val pick3 = next(pick2)

    val destination =
      Iterator.iterate(current)(i => (i - 1 - 1) %+ max + 1)
        .find(i => i != current && i != pick1 && i != pick2 && i != pick3)
        .get

    val newNext = next + (current -> next(pick3)) + (destination -> pick1) + (pick3 -> next(destination))
    State(newNext, newNext(current), max)
  }

  def simulateMoves(state: State, moves: Int = 100): State = {
    Iterator.iterate(state)(simulateMove)(moves)
  }

  def cups2State(cups: Cups): State = {
    val next =
      cups
        .iterator
        .zipWithTail
        .toMap
    val nextLoop = next + (cups.last -> cups.head)
    State(nextLoop, cups.head, cups.max)
  }

  def simulateMovesLabels(cups: Cups, moves: Int = 100): String = {
    val finalState = simulateMoves(cups2State(cups), moves)
    finalState.toCups(1).tail.mkString("")
  }

  def simulateMovesPart2(cups: Cups, moves: Int = 10000000): State = {
    val newCups = cups ++ ((cups.max + 1) to 1000000)
    simulateMoves(cups2State(newCups), moves)
  }

  def simulateMovesLabelsPart2(cups: Cups, moves: Int = 10000000): Long = {
    val finalState = simulateMovesPart2(cups, moves)
    finalState.toCups(1).tail.take(2).map(_.toLong).tapEach(println).product
  }


  def parseCups(input: String): Cups = input.map(_.asDigit).toVector

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim
  val input: String = "318946572"

  def main(args: Array[String]): Unit = {
    println(simulateMovesLabels(parseCups(input)))
    println(simulateMovesLabelsPart2(parseCups(input)))
  }
}
