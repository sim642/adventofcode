package eu.sim642.adventofcode2016

import eu.sim642.adventofcode2018.Day2.HeadIterator

object Day14 {

  def keys(salt: String): Iterator[(Int, String)] = {
    Iterator.from(0)
      .map(i => (i, Day5.md5(salt + i)))
      .sliding(1 + 1000)
      .filter({ case (i, hash) +: next1000 =>
        hash.sliding(3).find(s3 => s3(0) == s3(1) && s3(1) == s3(2)) match {
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
    keys(salt).drop(64 - 1).head._1
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim
  val input = "qzyelonm"

  def main(args: Array[String]): Unit = {
    println(key64Index(input))
  }
}
