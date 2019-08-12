package eu.sim642.adventofcode2016

import java.security.MessageDigest

import eu.sim642.adventofcode2018.Day2.HeadIterator

object Day14 {

  // https://old.reddit.com/r/adventofcode/comments/5i8pzz/2016_day_14_solutions/dbd7lvz/
  // https://stackoverflow.com/a/9855338/854540
  private val HEX_ARRAY = "0123456789abcdef".toCharArray
  def bytesToHex(bytes: Array[Byte]): String = {
    val hexChars = new Array[Char](bytes.length * 2)
    for (j <- bytes.indices) {
      val v = bytes(j) & 0xFF
      hexChars(j * 2) = HEX_ARRAY(v >>> 4)
      hexChars(j * 2 + 1) = HEX_ARRAY(v & 0x0F)
    }
    new String(hexChars)
  }

  // https://stackoverflow.com/a/5992852
  def md5(s: String): String = {
    bytesToHex(MessageDigest.getInstance("MD5").digest(s.getBytes))
  }

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
      keys(salt).drop(64 - 1).head._1
    }
  }

  object Part1 extends Part {
    override def hash(s: String): String = md5(s)
  }

  object Part2 extends Part {
    override def hash(s: String): String = {
      Iterator.iterate(s)(md5).drop(2017).head
    }
  }

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim
  val input = "qzyelonm"

  def main(args: Array[String]): Unit = {
    println(Part1.key64Index(input))
    println(Part2.key64Index(input))
  }
}
