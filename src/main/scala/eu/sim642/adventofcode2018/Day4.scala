package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2018.Day2.GroupCountIterable

object Day4 {

  sealed trait Event
  case class BeginShift(guard: Int) extends Event
  case object FallAsleep extends Event
  case object WakeUp extends Event

  private val minuteRegex = """\d{4}-\d{2}-\d{2} \d{2}:(\d{2})""".r

  case class Record(timestamp: String, event: Event) {
    def minute: Int = timestamp match {
      case minuteRegex(minute) => minute.toInt
    }
  }

  private val recordRegex = """\[(.{16})\] (.*)""".r
  private val beginShiftRegex = """Guard #(\d+) begins shift""".r

  def parseRecord(s: String): Record = s match {
    case recordRegex(timestamp, event) => Record(timestamp, event match {
      case beginShiftRegex(guard) => BeginShift(guard.toInt)
      case "falls asleep" => FallAsleep
      case "wakes up" => WakeUp
    })
  }

  def parseRecords(input: String): List[Record] = input.linesIterator.map(parseRecord).toList.sortBy(_.timestamp)

  case class Shift(guard: Int, sleep: Set[Int])

  def parseShifts(records: List[Record], shiftOption: Option[Shift] = None, sleepOption: Option[Int] = None): List[Shift] = records match {
    case Nil => shiftOption.toList
    case record :: tl => record.event match {
      case BeginShift(guard) =>
        shiftOption.toList ++ parseShifts(tl, Some(Shift(guard, Set.empty)))
      case FallAsleep =>
        parseShifts(tl, shiftOption, Some(record.minute))
      case WakeUp =>
        parseShifts(tl, shiftOption.map({ case Shift(guard, sleep) => Shift(guard, sleep ++ (sleepOption.get until record.minute).toSet) }))
    }
  }

  trait Strategy {
    def choose(shifts: List[Shift]): Int
    def choose(input: String): Int = choose(parseShifts(parseRecords(input)))
  }

  object Strategy1 extends Strategy {
    def choose(shifts: List[Shift]): Int = {
      val guardSleeps = shifts.groupMapReduce(_.guard)(_.sleep.toSeq)(_ ++ _)
      val (guard, sleeps) = guardSleeps.maxBy(_._2.length)
      val minute = (0 until 60).maxBy(minute => sleeps.count(_ == minute))
      guard * minute
    }
  }

  object Strategy2 extends Strategy {
    override def choose(shifts: List[Shift]): Int = {
      val guardSleeps = shifts.groupMapReduce(_.guard)(_.sleep.toSeq)(_ ++ _)
      val guardMinuteCount = guardSleeps.view.mapValues(_.groupCount(identity)).toMap
      val minuteMaxGuardCount = (0 until 60).map(minute => minute -> guardMinuteCount.view.mapValues(_.getOrElse(minute, 0)).maxBy(_._2)).toMap
      val (minute, (guard, count)) = minuteMaxGuardCount.maxBy(_._2._2)
      minute * guard
    }
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Strategy1.choose(input))
    println(Strategy2.choose(input))
  }
}
