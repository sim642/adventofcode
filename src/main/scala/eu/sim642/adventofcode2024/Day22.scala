package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day22 {

  type Secret = Long

  def mix(secret: Secret, value: Secret): Secret = value ^ secret
  def prune(secret: Secret): Secret = secret & 0xFFFFFF // % 16777216

  def nextSecret(secret: Secret): Secret = {
    val secret1 = prune(mix(secret, secret << 6)) // * 64
    val secret2 = mix(secret1, secret1 >> 5) // / 32, no prune needed after right shift
    prune(mix(secret2, secret2 << 11)) // * 2048
  }

  def secretIterator(initialSecret: Secret): Iterator[Secret] =
    Iterator.iterate(initialSecret)(nextSecret)

  def sumSecretsAfter(secrets: Seq[Secret], after: Int = 2000): Secret =
    secrets.map(secretIterator(_)(after)).sum

  def mostBananas(secrets: Seq[Secret]): Int = {
    // TODO: optimize (~4.7s)
    val secretMaps = secrets
      .map({ initialSecret =>
        secretIterator(initialSecret).take(2000 + 1)
          .map(_ % 10)
          .map(_.toInt)
          .sliding(5)
          .map({ prices =>
            val changes = (prices lazyZip prices.tail).map((a, b) => b - a)
            changes -> prices.last
          })
          .groupMapReduce(_._1)(_._2)((a, _) => a)
      })
    val secretMaps2 = secretMaps
      .flatten
      .groupMapReduce(_._1)(_._2)(_ + _)
    secretMaps2.values.max
  }

  def parseSecrets(input: String): Seq[Secret] = input.linesIterator.map(_.toLong).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumSecretsAfter(parseSecrets(input)))
    println(mostBananas(parseSecrets(input)))
  }
}
