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

  // Hacky solution that assumes unique names
  /*def dirSizes(commands: Seq[Command]): Map[String, Int] = {

    @tailrec
    def helper(commands: List[Command], acc: Map[String, Int]): Map[String, Int] = commands match {
      case Nil => acc
      case Command.Ls(results) :: Command.Cd(dir) :: newCommands =>
        val size =
          results
            .map({
              case LsResult.File(_, size) => size
              case LsResult.Dir(name) => acc(name)
            })
            .sum
        println(dir)
        assert(!acc.contains(dir))
        val newAcc = acc + (dir -> size)
        helper(newCommands, newAcc)
      case Command.Cd(".." | "/") :: newCommands =>
        helper(newCommands, acc)
    }

    helper(commands.reverse.toList, Map.empty)
  }*/

  enum Fs {
    case File(size: Int)
    case Dir(items: Map[String, Fs])
  }

  case class FsZipper(current: Map[String, Fs], context: Option[FsZipperContext]) {
    def up: FsZipper = {
      val FsZipperContext(name, siblings, parent) = context.get
      val newCurrent = siblings + (name -> Fs.Dir(current))
      FsZipper(newCurrent, parent)
    }

    @tailrec
    final def top: FsZipper = context match {
      case None => this
      case Some(_) => up.top
    }

    def down(name: String): FsZipper = {
      val newCurrent = current(name) match {
        case Fs.Dir(items) => items
        case Fs.File(_) => throw new IllegalArgumentException("invalid cd to file")
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
                    name -> Fs.File(size)
                  case LsResult.Dir(name) =>
                    name -> Fs.Dir(Map.empty)
                })
                .toMap
            acc.copy(current = newCurrent)
        }
      })
    Fs.Dir(finalZipper.top.current)
  }

  def totalSmallDirSizes(commands: Seq[Command]): Int = {
    // Hacky solution
    /*dirSizes(commands)
      .filter(_._2 <= 100_000)
      .values
      .sum*/

    def helper(fs: Fs): (Int, Int) = fs match {
      case Fs.File(size) => (size, 0)
      case Fs.Dir(items) =>
        val helperItems = items.values.map(helper)
        val size = helperItems.map(_._1).sum
        val totalSmallSize = helperItems.map(_._2).sum + (if (size <= 100_000) size else 0)
        (size, totalSmallSize)
    }

    helper(interpretCommands(commands))._2
  }

  def findDeleteDirSize(commands: Seq[Command]): Int = {
    // TODO: deduplicate
    def helper(fs: Fs): (Int, Int) = fs match {
      case Fs.File(size) => (size, 0)
      case Fs.Dir(items) =>
        val helperItems = items.values.map(helper)
        val size = helperItems.map(_._1).sum
        val totalSmallSize = helperItems.map(_._2).sum + (if (size <= 100_000) size else 0)
        (size, totalSmallSize)
    }

    val fs = interpretCommands(commands)
    val totalSize = helper(fs)._1
    val unusedSize = 70000000 - totalSize

    def helper2(fs: Fs): (Int, Option[Int]) = fs match {
      case Fs.File(size) => (size, None)
      case Fs.Dir(items) =>
        val helperItems = items.values.map(helper2)
        val size = helperItems.map(_._1).sum
        val smallestDeleteSize = helperItems.flatMap(_._2).minOption
        val newSmallestDeleteSize =
          if (unusedSize + size >= 30000000) {
            smallestDeleteSize match {
              case None => Some(size)
              case Some(smallest) => Some(smallest min size)
            }
          }
          else
            None
        (size, newSmallestDeleteSize)
    }

    helper2(fs)._2.get
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

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalSmallDirSizes(parseCommands(input)))
    println(findDeleteDirSize(parseCommands(input)))

    // part 1: 756542 - too low (hacky solution)
  }
}
