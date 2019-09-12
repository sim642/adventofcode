package eu.sim642.adventofcode2016

import eu.sim642.adventofcodelib.Hash
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day14 {

  trait Part {
    def hash(s: String): String

    def keys(salt: String): Iterator[(Int, String)] = {
      Iterator.from(0)
        .map(i => (i, hash(salt + i)))
        .sliding(1 + 1000)
        .filter({ case (i, hash) +: next1000 =>
          hash.toSeq.sliding(3).find(s3 => s3(0) == s3(1) && s3(1) == s3(2)) match {
            case None =>
              false
            case Some(s3) =>
              val c = s3.head
              val s5 = s"$c$c$c$c$c"
              next1000.exists(_._2.contains(s5))
          }
        })
        .map(_.head)
    }

    def key64Index(salt: String): Int = {
      keys(salt)(64 - 1)._1
    }
  }

  object Part1 extends Part {
    override def hash(s: String): String = Hash.md5(s)
  }

  object Part2 extends Part {
    override def hash(s: String): String = {
      Iterator.iterate(s)(Hash.md5)(2017)
    }
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim
  val input = "qzyelonm"

  def main(args: Array[String]): Unit = {
    println(Part1.key64Index(input))
    println(Part2.key64Index(input))
  }
}
