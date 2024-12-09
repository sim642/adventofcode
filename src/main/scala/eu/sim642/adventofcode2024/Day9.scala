package eu.sim642.adventofcode2024

import eu.sim642.adventofcode2024.Day9.Block.*

import scala.annotation.tailrec

object Day9 {

  enum Block {
    case File(id: Int, size: Int)
    case Free(size: Int)
  }

  type Filesystem = Vector[Block]

  def defragment(filesystem: Filesystem): Filesystem = {

    @tailrec
    def helper(filesystem: Filesystem, acc: Filesystem): Filesystem = {
      filesystem match {
        case (file@File(_, _)) +: newFilesystem => helper(newFilesystem, file +: acc)
        case (newFileSystem@Free(_) +: _) :+ Free(_) => helper(newFileSystem, acc)
        case Free(freeSize) +: newFileSystem :+ (file@File(id, fileSize)) =>
          if (freeSize == fileSize)
            helper(newFileSystem, file +: acc)
          else if (freeSize > fileSize)
            helper(Free(freeSize - fileSize) +: newFileSystem, file +: acc)
          else // freeSize < fileSize
            helper(newFileSystem :+ File(id, fileSize - freeSize), File(id, freeSize) +: acc)
        case Seq(Free(_)) | Seq() => acc.reverse
      }
    }

    helper(filesystem, Vector.empty)
  }

  def checksum(filesystem: Filesystem): Long = {
    filesystem.foldLeft((0L, 0))({ case ((acc, i), block) =>
      block match {
        case File(id, size) => (acc + id * (i until i + size).sum.toLong, i + size)
        case Free(size) => (acc, i + size)
      }
    })._1
  }

  def defragmentChecksum(filesystem: Filesystem): Long = checksum(defragment(filesystem))

  def parseFilesystem(input: String): Filesystem = {
    input.view.zipWithIndex.map({ case (size, i) =>
      if (i % 2 == 0)
        File(i / 2, size.asDigit)
      else
        Free(size.asDigit)
    }).toVector
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(defragmentChecksum(parseFilesystem(input)))

    // part 1: 1368861652 - too low (Int overflowed in checksum)
  }
}
