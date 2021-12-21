package eu.sim642.adventofcode2021

import scala.annotation.tailrec


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
    def helper(p1: Player, p2: Player, i: Int, dice: LazyList[Int]): (Players, Int) = {
      val (roll, newDice) = dice.splitAt(3)
      val newI = i + 3
      val newP1 = p1.move(roll.sum)
      if (newP1.score >= 1000)
        ((newP1, p2), newI)
      else
        helper(p2, newP1, newI, newDice)
    }

    val dice = LazyList.continually(LazyList.from(1).take(100)).flatten
    helper(players._1, players._2, 0, dice)
  }

  def loserScoreRolls(players: Players): Int = {
    val ((_, loser), rolls) = play(players)
    loser.score * rolls
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
  }
}
