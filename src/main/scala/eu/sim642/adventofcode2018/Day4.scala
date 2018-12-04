package eu.sim642.adventofcode2018

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

  def parseRecords(input: String): List[Record] = input.lines.map(parseRecord).toList.sortBy(_.timestamp)

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

  def strategy1(shifts: List[Shift]): Int = {
    val guardSleeps = shifts.groupBy(_.guard).mapValues(_.map(_.sleep.toSeq).reduce(_ ++ _))
    val (guard, sleeps) = guardSleeps.maxBy(_._2.length)
    val minute = (0 until 60).maxBy(minute => sleeps.count(_ == minute))
    guard * minute
  }

  def strategy1(input: String): Int = strategy1(parseShifts(parseRecords(input)))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(strategy1(input))
  }
}
