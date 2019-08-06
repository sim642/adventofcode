package eu.sim642.adventofcode2018

import scala.annotation.tailrec

object Day9 {

  /**
    * Zipper-like immutable circular buffer.
    */
  case class CircularZipper[A](init: List[A], current: A, tail: List[A]) {
    def next: CircularZipper[A] = tail match {
      case hd :: tl => CircularZipper(current :: init, hd, tl)
      case Nil =>
        init.reverse match {
          case hd :: it => CircularZipper(List(current), hd, it)
          case Nil => this
        }
    }

    def prev: CircularZipper[A] = init match {
      case hd :: it => CircularZipper(it, hd, current :: tail)
      case Nil =>
        tail.reverse match {
          case hd :: tl => CircularZipper(tl, hd, List(current))
          case Nil => this
        }
    }

    @tailrec
    final def rotate(n: Int): CircularZipper[A] = {
      if (n == 0)
        this
      else if (n > 0)
        next.rotate(n - 1)
      else
        prev.rotate(n + 1)
    }

    def inserted(elem: A): CircularZipper[A] = CircularZipper(init, elem, current :: tail)

    def removed: (A, CircularZipper[A]) = tail match {
      case hd :: tl => (current, CircularZipper(init, hd, tl))
      case Nil =>
        val hd :: it = init
        (current, CircularZipper(it, hd, tail)) // TODO: fix new current coming from different size?
    }
    /*def removed: (A, CircularZipper[A]) = {
      val CircularZipper(elem :: init, current, tail) = next
      (elem, CircularZipper(init, current, tail))
    }*/
  }

  object CircularZipper {
    def apply[A](elem: A): CircularZipper[A] = CircularZipper(Nil, elem, Nil)
  }

  def highscore(playerCount: Int, lastMarble: Int): Long = {
    def helper(marbles: CircularZipper[Int], marble: Int, player: Int, scores: Map[Int, Long]): Map[Int, Long] = {
      val nextPlayer = ((player - 1) + 1) % playerCount + 1

      if (marble > lastMarble)
        scores
      else if (marble % 23 == 0) {
        val (removed, newMarbles) = marbles.rotate(-7).removed
        val newScores = scores.updated(player, scores(player) + marble + removed)
        helper(newMarbles, marble + 1, nextPlayer, newScores)
      }
      else {
        val newMarbles = marbles.rotate(2).inserted(marble)
        helper(newMarbles, marble + 1, nextPlayer, scores)
      }
    }

    helper(CircularZipper(0), 1, 1, Map.empty.withDefaultValue(0)).values.max
  }

  def highscore(input: String, lastMarbleMult: Int = 1): Long = {
    val (playerCount, lastMarble) = parseInput(input)
    highscore(playerCount, lastMarble * lastMarbleMult)
  }

  private val inputRegex = """(\d+) players; last marble is worth (\d+) points""".r

  def parseInput(input: String): (Int, Int) = input match {
    case inputRegex(playerCount, lastMarble) => (playerCount.toInt, lastMarble.toInt)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(highscore(input))
    println(highscore(input, lastMarbleMult = 100))
  }
}
