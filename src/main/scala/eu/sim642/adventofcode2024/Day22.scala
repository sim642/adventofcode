package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day22 {

  type Secret = Long

  def mix(secret: Secret, value: Secret): Secret = value ^ secret
  def prune(secret: Secret): Secret = secret % 16777216 // TODO: bitwise

  def nextSecret(secret: Secret): Secret = {
    val secret1 = prune(mix(secret, secret * 64))
    val secret2 = prune(mix(secret1, secret1 / 32))
    prune(mix(secret2, secret2 * 2048))
  }

  def secretAfter(initialSecret: Secret, after: Int = 2000): Secret =
    Iterator.iterate(initialSecret)(nextSecret)(after)

  def sumSecretsAfter(secrets: Seq[Secret], after: Int = 2000): Secret =
    secrets.map(secretAfter(_, after)).sum

  def parseSecrets(input: String): Seq[Secret] = input.linesIterator.map(_.toLong).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumSecretsAfter(parseSecrets(input)))
  }
}
