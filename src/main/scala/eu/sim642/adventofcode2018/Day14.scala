package eu.sim642.adventofcode2018

object Day14 {

  case class RecipeState(scores: Vector[Int], index1: Int, index2: Int) {
    def next: RecipeState = {
      val score1 = scores(index1)
      val score2 = scores(index2)
      val sum = score1 + score2
      val newScores = {
        if (sum >= 10)
          scores ++ Vector(sum / 10, sum % 10)
        else
          scores :+ sum
      }
      val newIndex1 = (index1 + (1 + score1)) % newScores.length
      val newIndex2 = (index2 + (1 + score2)) % newScores.length
      RecipeState(newScores, newIndex1, newIndex2)
    }
  }

  object RecipeState {
    val initial = RecipeState(Vector(3, 7), 0, 1)
  }

  def tenScores(after: Int): String = {
    val it = Iterator.iterate(RecipeState.initial)(_.next)
    it.find(_.scores.length >= after + 10).get.scores.slice(after, after + 10).mkString("")
  }

  def recipesToLeft(scores: String): Int = {
    val scoreSeq = scores.map(_.asDigit)
    val it = Iterator.iterate(RecipeState.initial)(_.next)
    var prevStateLength = 0 // TODO: don't use this state
    it.find({ state =>
      val i = state.scores.indexOfSlice(scoreSeq, prevStateLength - scoreSeq.length)
      prevStateLength = state.scores.length
      i >= 0
    }).get.scores.indexOfSlice(scoreSeq)
  }

  def recipesToLeft(scores: Int): Int = recipesToLeft(scores.toString)

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim
  val input = 846021

  def main(args: Array[String]): Unit = {
    println(tenScores(input))
    println(recipesToLeft(input))
  }
}
