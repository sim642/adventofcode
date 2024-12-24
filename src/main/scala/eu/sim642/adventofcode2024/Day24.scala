package eu.sim642.adventofcode2024

import scala.collection.mutable

object Day24 {

  enum Op {
    case And
    case Or
    case Xor
  }

  enum Wire {
    case Input(value: Boolean)
    case Gate(lhs: String, op: Op, rhs: String)
  }

  type Circuit = Map[String, Wire]

  def getZValue(circuit: Circuit): Long = {

    val memo = mutable.Map.empty[String, Boolean]

    def evalName(name: String): Boolean =
      memo.getOrElseUpdate(name, evalWire(circuit(name)))

    def evalWire(wire: Wire): Boolean = wire match {
      case Wire.Input(value) => value
      case Wire.Gate(lhs, op, rhs) =>
        val left = evalName(lhs)
        val right = evalName(rhs)
        op match {
          case Op.And => left && right
          case Op.Or => left || right
          case Op.Xor => left != right
        }
    }

    circuit.keys
      .filter(_.startsWith("z"))
      .toSeq
      .sorted
      .foldRight(0L)({ case (zName, acc) =>
        acc << 1 | (if (evalName(zName)) 1 else 0)
      })
  }

  def parseInput(s: String): (String, Wire.Input) = s match {
    case s"$name: 0" => name -> Wire.Input(false)
    case s"$name: 1" => name -> Wire.Input(true)
  }

  def parseGate(s: String): (String, Wire.Gate) = s match {
    case s"$lhs AND $rhs -> $name" => name ->Wire.Gate(lhs, Op.And, rhs)
    case s"$lhs OR $rhs -> $name" => name -> Wire.Gate(lhs, Op.Or, rhs)
    case s"$lhs XOR $rhs -> $name" => name -> Wire.Gate(lhs, Op.Xor, rhs)
  }

  def parseCircuit(input: String): Circuit = input match {
    case s"$inputs\n\n$gates" =>
      val inputMap = inputs.linesIterator.map(parseInput).toMap
      val gateMap = gates.linesIterator.map(parseGate).toMap
      inputMap ++ gateMap
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(getZValue(parseCircuit(input)))
  }
}
