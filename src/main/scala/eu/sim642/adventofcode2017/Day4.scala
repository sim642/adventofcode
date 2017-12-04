package eu.sim642.adventofcode2017

object Day4 {

  def isValidPassphrase(words: Seq[String]): Boolean = words.distinct == words

  def isValidPassphrase(passphrase: String): Boolean = isValidPassphrase(passphrase.split("\\s+"))

  def countValidPassphrases(passphrases: Seq[String]): Int = passphrases.count(isValidPassphrase)

  val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim
  val inputLines: Seq[String] = input.lines.toSeq

  def main(args: Array[String]): Unit = {
    println(countValidPassphrases(inputLines))
  }
}
