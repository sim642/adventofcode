package eu.sim642.adventofcode2024

import eu.sim642.adventofcode2024.Day9.Block.*

import scala.annotation.tailrec

object Day9 {

  enum Block {
    case File(id: Int, size: Int)
    case Free(size: Int)
  }

  type Filesystem = Vector[Block]

  def checksum(filesystem: Filesystem): Long = {
    filesystem.foldLeft((0L, 0))({ case ((acc, i), block) =>
      block match {
        case File(id, size) => (acc + id * (i until i + size).sum.toLong, i + size)
        case Free(size) => (acc, i + size)
      }
    })._1
  }

  trait Part {
    def compact(filesystem: Filesystem): Filesystem

    def compactChecksum(filesystem: Filesystem): Long = checksum(compact(filesystem))
  }

  object Part1 extends Part {
    override def compact(filesystem: Filesystem): Filesystem = {

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
  }

  object Part2 extends Part {
    override def compact(filesystem: Filesystem): Filesystem = {

      @tailrec
      def helper(filesystem: Filesystem, acc: Filesystem): Filesystem = {
        filesystem match {
          case newFilesystem :+ (free@Free(_)) => helper(newFilesystem, free +: acc)
          case newFilesystem :+ (file@File(id, fileSize)) =>
            val moveIndex = newFilesystem.indexWhere({
              case Free(freeSize) => freeSize >= fileSize
              case File(_, _) => false
            })
            if (moveIndex >= 0) {
              val (before, Free(freeSize) +: after) = newFilesystem.splitAt(moveIndex): @unchecked // indexWhere+splitAt is faster than span
              val replace =
                if (freeSize == fileSize)
                  Vector(file)
                else
                  Vector(file, Free(freeSize - fileSize))
              helper(before ++ replace ++ after, Free(fileSize) +: acc)
            }
            else
              helper(newFilesystem, file +: acc)
          case Seq() => acc
        }
      }

      helper(filesystem, Vector.empty)
    }
  }


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
    println(Part1.compactChecksum(parseFilesystem(input)))
    println(Part2.compactChecksum(parseFilesystem(input)))

    // part 1: 1368861652 - too low (Int overflowed in checksum)
  }
}
