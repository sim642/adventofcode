package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IntegralImplicits._
import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day25 {

  private val modulo: Int = 20201227

  def transformSubjectNumber(subjectNumber: Int, loopSize: Int): Int = {
    subjectNumber.toLong.modPow(loopSize, modulo).toInt
  }

  // TODO: move discrete log to NumberTheory

  def discreteLogNaive(a: Int, b: Int, n: Int): Int = {
    Iterator.iterate(1)(value => (a * value) % n)
      .zipWithIndex
      .find(_._1 == b)
      .get
      ._2
  }

  def discreteLogBabyStepGiantStep(a: Int, b: Int, n: Int): Int = {
    // https://en.wikipedia.org/wiki/Baby-step_giant-step
    val m = math.sqrt(n).ceil.toInt
    val map = {
      Iterator.iterate(1, m)(acc => ((a * acc.toLong) % n).toInt)
        .zipWithIndex
        .toMap
    }

    val amm = NumberTheory.modInv(a.toLong, n.toLong).modPow(m, n).toInt
    Iterator.iterate(b, m)(acc => ((amm * acc.toLong) % n).toInt)
      .zipWithIndex
      .flatMap({ case (gamma, i) =>
        map.get(gamma).map(j => i * m + j)
      })
      .head
  }

  def findLoopSize(publicKey: Int, subjectNumber: Int = 7): Int = {
    //discreteLogNaive(subjectNumber, publicKey, modulo)
    discreteLogBabyStepGiantStep(subjectNumber, publicKey, modulo)
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
