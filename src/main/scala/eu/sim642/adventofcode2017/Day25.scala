package eu.sim642.adventofcode2017

object Day25 {

  type State = Char
  type Symbol = Int

  sealed trait Move
  case object Left extends Move
  case object Right extends Move

  type Transitions = Map[(State, Symbol), (State, Symbol, Move)]

  type Position = Int
  type Tape = Map[Position, Symbol]

  case class Configuration(transitions: Transitions, state: State,
                           cursor: Position = 0, tape: Tape = Map.empty.withDefaultValue(0)) {

    def step: Configuration = {
      val (newState, newSymbol, move) = transitions((state, tape(cursor)))

      Configuration(transitions, newState, cursor + (move match {
          case Left => -1
          case Right => 1
        }), tape + (cursor -> newSymbol))
    }
  }

  private val initialStateRegex = """Begin in state ([A-Z]).""".r
  private val diagnosticRegex = """Perform a diagnostic checksum after (\d+) steps.""".r
  private val transitionStateRegex = """(?s)In state ([A-Z]):\n(.*?)(?:\n\n|$)""".r
  private val transitionValueRegex = """  If the current value is (\d):
                                       |    - Write the value (\d).
                                       |    - Move one slot to the (left|right).
                                       |    - Continue with state ([A-Z]).""".stripMargin.r

  def parseConfiguration(input: String): Configuration = {
    val initialStateM = initialStateRegex.findFirstMatchIn(input).get

    def parseStateTransitions(state: State, stateInput: String): Transitions = {
      transitionValueRegex.findAllMatchIn(stateInput).map(m => {
        (state, m.group(1).toInt) -> (m.group(4).head, m.group(2).toInt, m.group(3) match {
          case "left" => Left
          case "right" => Right
        })
      }).toMap
    }

    val transitions = transitionStateRegex.findAllMatchIn(input).map(m => parseStateTransitions(m.group(1).head, m.group(2))).reduce(_ ++ _)

    Configuration(transitions, initialStateM.group(1).head)
  }

  def iterateTuringMachine(initialConfiguration: Configuration): Iterator[Configuration] =
    Iterator.iterate(initialConfiguration)(_.step)

  def diagnosticChecksum(initialConfiguration: Configuration, steps: Int): Int =
    iterateTuringMachine(initialConfiguration).drop(steps).next().tape.count(_._2 == 1)

  def diagnosticChecksum(input: String): Int = {
    val diagnosticM = diagnosticRegex.findFirstMatchIn(input).get
    diagnosticChecksum(parseConfiguration(input), diagnosticM.group(1).toInt)
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day25.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(diagnosticChecksum(input))
  }
}
