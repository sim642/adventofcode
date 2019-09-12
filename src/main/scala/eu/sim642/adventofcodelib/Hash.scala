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
  def md5(s: String): String = {
    bytesToHex(MessageDigest.getInstance("MD5").digest(s.getBytes))
  }
}
