package eu.sim642.adventofcodelib

import eu.sim642.adventofcodelib.IntegralImplicits._

object SeqImplicits {

  implicit class RotateSeqOps[A](seq: Seq[A]) {
    def rotateLeft(n: Int): Seq[A] = {
      val realN = n %+ seq.length
      val (init, tail) = seq.splitAt(realN)
      tail ++ init
    }

    def rotateRight(n: Int): Seq[A] = rotateLeft(-n)
  }

  // TODO: in Scala 2.13 remove and use IsSeq or something
  implicit class RotateStringOps(s: String) {
    def rotateLeft(n: Int): String = {
      val realN = n %+ s.length
      val (init, tail) = s.splitAt(realN)
      tail + init
    }

    def rotateRight(n: Int): String = rotateLeft(-n)
  }
}
