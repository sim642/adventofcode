package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._
import eu.sim642.adventofcodelib.graph.{BFS, GraphSearch, UnitNeighbors}
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day25 {

  def runInteractive(program: Memory): Unit = {
    val reader = Console.in
    val inputs = LazyList.unfold(())({ _ =>
      val c = reader.read()
      if (c < 0)
        None
      else
        Some((c.toLong, ()))
    })

    val outputs = ProgramState(program, inputs = inputs).outputs
    outputs.foreach(c => print(c.toChar))
  }

  private val roomRegex =
    """== (.+) ==
      |.*
      |
      |Doors here lead:
      |((?:- \w+
      |)+)(?:
      |Items here:
      |((?:- [\w ]+
      |)+))?
      |(?:Command\?|(?:.|\n)*?(\d+))""".stripMargin.r.unanchored
  private val listItemRegex = """- ([\w ]+)""".r

  private val takeRegex = """You take the ([\w ]+)\.""".r.unanchored
  private val dropRegex = """You drop the ([\w ]+)\.""".r.unanchored

  case class DroidState(room: String,
                        doors: Set[String],
                        items: Set[String],
                        inventory: Set[String],
                        password: Option[Int])
                       (val programState: ProgramState) {

    def runCommand(command: String): DroidState = {
      val inputs = (command + "\n").map(_.toLong).to(LazyList)
      val outputStates = programState.copy(inputs = inputs).outputStates
      val outputString = outputStates.map(_._2.toChar).mkString
      Console.err.println(outputString)
      val newProgramState = outputStates.last._1

      outputString match {
        case roomRegex(room, doors, items, password2) =>
          val doorsSet = listItemRegex.findAllMatchIn(doors).map(_.group(1)).toSet
          val itemsSet = {
            if (items == null)
              Set.empty[String]
            else
              listItemRegex.findAllMatchIn(items).map(_.group(1)).toSet
          }
          DroidState(room, doorsSet, itemsSet, inventory, password.orElse(Option(password2).map(_.toInt)))(newProgramState)
        case takeRegex(item) =>
          DroidState(room, doors, items - item, inventory + item, password)(newProgramState)
        case dropRegex(item) =>
          DroidState(room, doors, items + item, inventory - item, password)(newProgramState)
      }
    }

    def move(door: String): DroidState = {
      require(doors.contains(door))
      runCommand(door)
    }

    def take(item: String): DroidState = {
      require(items.contains(item))
      runCommand(s"take $item")
    }

    def drop(item: String): DroidState = {
      require(inventory.contains(item))
      runCommand(s"drop $item")
    }
  }

  object DroidState {
    // TODO: reduce duplication
    def apply(programState: ProgramState): DroidState = {
      val outputStates = programState.outputStates
      val outputString = outputStates.map(_._2.toChar).mkString
      Console.err.println(outputString)
      val newProgramState = outputStates.last._1

      outputString match {
        case roomRegex(room, doors, items, password2) =>
          val doorsSet = listItemRegex.findAllMatchIn(doors).map(_.group(1)).toSet
          val itemsSet = {
            if (items == null)
              Set.empty[String]
            else
              listItemRegex.findAllMatchIn(items).map(_.group(1)).toSet
          }
          DroidState(room, doorsSet, itemsSet, Set.empty, Option(password2).map(_.toInt))(newProgramState)
      }
    }
  }

  private val badItems = Set(
    "escape pod",
    "infinite loop",
    "giant electromagnet",
    "photons",
    "molten lava",
  )

  private val oppositeDoors = Map(
    "north" -> "south",
    "south" -> "north",
    "east" -> "west",
    "west" -> "east",
  )

  private val securityRoom = "Security Checkpoint"

  // DFS
  def collectItems(droidState: DroidState, excludeDoor: Option[String] = None): DroidState = {
    val droidStateItems = droidState.items.filterNot(badItems).foldLeft(droidState)(_.take(_))
    val droidStateMoves = {
      if (droidState.room == securityRoom)
        droidStateItems // don't try to go to pressure-sensitive floor room
      else {
        droidStateItems.doors.filterNot(excludeDoor.contains).foldLeft(droidStateItems)({ (droidState, door) =>
          val oppositeDoor = oppositeDoors(door)
          collectItems(droidState.move(door), Some(oppositeDoor)).move(oppositeDoor)
        })
      }
    }
    droidStateMoves
  }

  def goToSecurityRoom(droidState: DroidState): DroidState = {
    val graphSearch = new GraphSearch[DroidState] with UnitNeighbors[DroidState] {
      override val startNode: DroidState = droidState

      override def unitNeighbors(droidState: DroidState): IterableOnce[DroidState] =
        droidState.doors.iterator.map(droidState.move)

      override def isTargetNode(droidState: DroidState, dist: Address): Boolean =
        droidState.room == securityRoom
    }

    BFS.search(graphSearch).target.get._1
  }

  def findPressureSensitiveFloorDoor(droidState: DroidState): String = {
    droidState.doors.find(droidState.move(_).room == securityRoom).get
  }

  def findPassword(program: Memory): Int = {

    val droidStateInitial = DroidState(ProgramState(program))
    val droidStateItems = collectItems(droidStateInitial)
    //println(droidStateItems)
    val droidStateSecurity = goToSecurityRoom(droidStateItems)
    //println(droidStateSecurity)
    val pressureSensitiveFloorDoor = findPressureSensitiveFloorDoor(droidStateSecurity)
    //println(pressureSensitiveFloorDoor)

    val password = droidStateSecurity.inventory.subsets.map({ dropItems =>
      val droidStateDropped = dropItems.foldLeft(droidStateSecurity)(_.drop(_))
      droidStateDropped.move(pressureSensitiveFloorDoor)
    }).flatMap(_.password).head

    password
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    //runInteractive(parseProgram(input))
    println(findPassword(parseProgram(input)))

    // whirled peas, fixed point, prime number, antenna
    // 2622472
  }
}
