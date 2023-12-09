package eu.sim642.adventofcode2022

import scala.annotation.tailrec

object Day7 {

  enum LsResult {
    case Dir(name: String)
    case File(name: String, size: Int)
  }

  enum Command {
    case Cd(dir: String)
    case Ls(results: Seq[LsResult])
  }

  sealed trait Fs {
    lazy val totalSize: Int
  }
  case class File(size: Int) extends Fs {
    override lazy val totalSize: Int = size
  }
  case class Dir(items: Map[String, Fs]) extends Fs {
    override lazy val totalSize: Int = items.values.map(_.totalSize).sum
  }

  /**
   * Zipper for Fs.
   */
  case class FsZipper(current: Map[String, Fs], context: Option[FsZipperContext]) {
    def up: FsZipper = {
      val FsZipperContext(name, siblings, parent) = context.get
      val newCurrent = siblings + (name -> Dir(current))
      FsZipper(newCurrent, parent)
    }

    @tailrec
    final def top: FsZipper = context match {
      case None => this
      case Some(_) => up.top
    }

    def down(name: String): FsZipper = {
      val newCurrent = current(name) match {
        case Dir(items) => items
        case File(_) => throw new IllegalArgumentException("invalid cd to file")
      }
      val newContext = FsZipperContext(name, current - name, context)
      FsZipper(newCurrent, Some(newContext))
    }
  }

  case class FsZipperContext(name: String, siblings: Map[String, Fs], parent: Option[FsZipperContext])

  def interpretCommands(commands: Seq[Command]): Fs = {
    val finalZipper = commands.foldLeft(FsZipper(Map.empty, None))({ (acc, command) =>
        command match {
          case Command.Cd("/") => acc.top
          case Command.Cd("..") => acc.up
          case Command.Cd(dir) => acc.down(dir)
          case Command.Ls(results) =>
            val newCurrent =
              results
                .map({
                  case LsResult.File(name, size) =>
                    name -> File(size)
                  case LsResult.Dir(name) =>
                    name -> Dir(Map.empty)
                })
                .toMap
            acc.copy(current = newCurrent)
        }
      })
    Dir(finalZipper.top.current)
  }

  def iterateDirs(fs: Fs): Iterator[Dir] = fs match {
    case File(_) =>
      Iterator.empty
    case dir@Dir(items) =>
      Iterator.single(dir) ++ items.values.iterator.flatMap(iterateDirs)
  }

  def totalSmallDirSizes(commands: Seq[Command]): Int = {
    val fs = interpretCommands(commands)
    iterateDirs(fs)
      .map(_.totalSize)
      .filter(_ <= 100_000)
      .sum
  }

  def findDeleteDirSize(commands: Seq[Command]): Int = {
    val fs = interpretCommands(commands)
    val unusedSize = 70_000_000 - fs.totalSize
    val missingSize = 30_000_000 - unusedSize
    iterateDirs(fs)
      .map(_.totalSize)
      .filter(_ >= missingSize)
      .min
  }


  def parseCommands(input: String): Seq[Command] = {
    input.linesIterator.foldLeft(List.empty[Command])({ (acc, line) =>
      line match {
        case s"$$ cd $dir" =>
          Command.Cd(dir) :: acc
        case s"$$ ls" =>
          Command.Ls(Nil) :: acc
        case line =>
          acc match {
            case Command.Ls(results) :: acc =>
              val newResult = line match {
                case s"dir $name" =>
                  LsResult.Dir(name)
                case s"$size $name" =>
                  LsResult.File(name, size.toInt)
              }
              Command.Ls(newResult +: results) :: acc
            case _ =>
              throw new IllegalStateException("invalid output after cd")
          }
      }
    }).reverse
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalSmallDirSizes(parseCommands(input)))
    println(findDeleteDirSize(parseCommands(input)))

    // part 1: 756542 - too low (hacky solution, assuming unique subdir names)
  }
}
