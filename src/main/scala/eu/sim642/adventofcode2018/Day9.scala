package eu.sim642.adventofcode2018

object Day9 {

  def highscore(playerCount: Int, lastMarble: Int): Int = {
    def helper(marbles: Vector[Int], currentIndex: Int, marble: Int, player: Int, scores: Map[Int, Int]): Map[Int, Int] = {
      val nextPlayer = ((player - 1) + 1) % playerCount + 1

      if (marble > lastMarble)
        scores
      else if (marble % 23 == 0) {
        val removeIndex = (currentIndex - 7 + marbles.length) % marbles.length
        val (init, removed +: tail) = marbles.splitAt(removeIndex)
        val newMarbles = init ++ tail
        val newScores = scores.updated(player, scores(player) + marble + removed)
        helper(newMarbles, removeIndex, marble + 1, nextPlayer, newScores)
      }
      else {
        val insertIndex = (currentIndex + 2) % marbles.length
        val (init, tail) = marbles.splitAt(insertIndex)
        val newMarbles: Vector[Int] = (init :+ marble) ++ tail
        helper(newMarbles, insertIndex, marble + 1, nextPlayer, scores)
      }
    }

    helper(Vector(0), 0, 1, 1, Map.empty.withDefaultValue(0)).values.max
  }

  def highscore(input: String): Int = {
    val (playerCount, lastMarble) = parseInput(input)
    highscore(playerCount, lastMarble)
  }

  private val inputRegex = """(\d+) players; last marble is worth (\d+) points""".r

  def parseInput(input: String): (Int, Int) = input match {
    case inputRegex(playerCount, lastMarble) => (playerCount.toInt, lastMarble.toInt)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(highscore(input))
  }
}
