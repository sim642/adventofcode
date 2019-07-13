package eu.sim642.adventofcode2017

object Day4 {

  trait Part {
    def isValidPassphrase(words: Seq[String]): Boolean

    def isValidPassphrase(passphrase: String): Boolean = isValidPassphrase(passphrase.split("\\s+"))

    def countValidPassphrases(passphrases: Seq[String]): Int = passphrases.count(isValidPassphrase)
  }

  object Part1 extends Part {
    override def isValidPassphrase(words: Seq[String]): Boolean = words.distinct == words
  }

  object Part2 extends Part {
    override def isValidPassphrase(words: Seq[String]): Boolean = {
      val sortedWords = words.map(_.sorted)
      Part1.isValidPassphrase(sortedWords)
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim
  lazy val inputLines: Seq[String] = input.linesIterator.toSeq

  def main(args: Array[String]): Unit = {
    println(Part1.countValidPassphrases(inputLines))
    println(Part2.countValidPassphrases(inputLines))
  }
}
