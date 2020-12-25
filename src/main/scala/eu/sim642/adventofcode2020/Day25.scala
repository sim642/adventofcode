package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IntegralImplicits._

object Day25 {

  private val modulo: Int = 20201227

  def transformSubjectNumber(subjectNumber: Int, loopSize: Int): Int = {
    subjectNumber.toLong.modPow(loopSize, modulo).toInt
  }

  def findLoopSize(publicKey: Int, subjectNumber: Int = 7): Int = {
    Iterator.iterate(1)(value => (subjectNumber * value) % modulo)
      .zipWithIndex
      .find(_._1 == publicKey)
      .get
      ._2
  }

  def findEncryptionKey(publicKeys: (Int, Int)): Int = {
    val (publicKey1, publicKey2) = publicKeys
    val loopSize1 = findLoopSize(publicKey1)
    transformSubjectNumber(publicKey2, loopSize1)
  }

  def parsePublicKeys(input: String): (Int, Int) = {
    val Seq(publicKey1, publicKey2) = input.linesIterator.take(2).map(_.toInt).toSeq
    (publicKey1, publicKey2)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findEncryptionKey(parsePublicKeys(input)))
  }
}
