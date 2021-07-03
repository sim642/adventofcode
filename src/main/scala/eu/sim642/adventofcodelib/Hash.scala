package eu.sim642.adventofcodelib

import java.security.MessageDigest

// TODO: better object name?
object Hash {

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
  trait DigestOps[A <: DigestOps[A]] {
    def apply(s: String): String
    def apply(): String
    def prefix(prefix: String): A
  }
  // TODO: implementation without clone()

  class Digest(messageDigest: MessageDigest) extends DigestOps[Digest] {
    private def cloneMessageDigest(): MessageDigest = messageDigest.clone().asInstanceOf[MessageDigest]

    override def apply(s: String): String = {
      val md = cloneMessageDigest()
      bytesToHex(md.digest(s.getBytes))
    }

    override def apply(): String = {
      val md = cloneMessageDigest()
      bytesToHex(md.digest())
    }

    override def prefix(prefix: String): Digest = {
      val md = cloneMessageDigest()
      md.update(prefix.getBytes)
      new Digest(md)
    }
  }

  class DigestFactory(algorithm: String) extends Digest(MessageDigest.getInstance(algorithm))

  // TODO: conditionally choose implementation without clone()
  object md5 extends DigestFactory("MD5")
}
