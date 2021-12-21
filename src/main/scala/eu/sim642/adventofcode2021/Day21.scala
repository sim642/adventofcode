package eu.sim642.adventofcode2021

import scala.annotation.tailrec
import eu.sim642.adventofcodelib.IterableImplicits.*

import scala.collection.mutable


object Day21 {

  case class Player(pos: Int, score: Int = 0) {
    def move(amount: Int): Player = {
      val newPos = (pos + amount - 1) % 10 + 1
      Player(newPos, score + newPos)
    }
  }

  type Players = (Player, Player)

  def play(players: Players): (Players, Int) = {

    @tailrec
    def helper(p1: Player, p2: Player, rolls: Int, dice: LazyList[Int]): (Players, Int) = {
      val (roll, newDice) = dice.splitAt(3)
      val newRolls = rolls + 3
      val newP1 = p1.move(roll.sum)
      if (newP1.score >= 1000)
        ((newP1, p2), newRolls)
      else
        helper(p2, newP1, newRolls, newDice)
    }

    val dice = LazyList.continually(LazyList.from(1).take(100)).flatten // TODO: LazyListImplicits
    helper(players._1, players._2, 0, dice)
  }

  def loserScoreRolls(players: Players): Int = {
    val ((_, loser), rolls) = play(players)
    loser.score * rolls
  }

  private val diracRollCounts = {
    (for {
      r1 <- 1 to 3
      r2 <- 1 to 3
      r3 <- 1 to 3
    } yield r1 + r2 + r3).groupCount(identity)
  }

  def diracPlay(players: Players): (Long, Long) = {

    val memo = mutable.Map.empty[Players, (Long, Long)]

    def helper(p1: Player, p2: Player): (Long, Long) = {
      memo.getOrElseUpdate((p1, p2), {
        if (p1.score >= 21)
          (1L, 0L)
        else if (p2.score >= 21)
          (0L, 1L)
        else {
          val rollsIt = for {
            (roll, rollCount) <- diracRollCounts.iterator
            newP1 = p1.move(roll)
            (p2Cnt, newP1Cnt) = helper(p2, newP1) // swapped
          } yield (rollCount * newP1Cnt, rollCount * p2Cnt)
          rollsIt.reduce({ case ((a, b), (c, d)) => (a + c, b + d) })
        }
      })
    }

    helper(players._1, players._2)
  }

  def winnerDiracUniverses(players: Players): Long = {
    val (p1Cnt, p2Cnt) = diracPlay(players)
    p1Cnt max p2Cnt
  }


  private val playersRegex =
    """Player 1 starting position: (\d+)
      |Player 2 starting position: (\d+)""".stripMargin.r

  def parsePlayers(input: String): Players = input match {
    case playersRegex(pos1, pos2) =>
      (Player(pos1.toInt), Player(pos2.toInt))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(loserScoreRolls(parsePlayers(input)))
    println(winnerDiracUniverses(parsePlayers(input)))
  }
}
