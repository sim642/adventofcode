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

  case class Circuit(wireMap: Map[String, Wire]) {
    def zValue: Long = {
      val memo = mutable.Map.empty[String, Boolean]

      def evalName(name: String): Boolean =
        memo.getOrElseUpdate(name, evalWire(wireMap(name)))

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

      wireMap.keys
        .filter(_.startsWith("z"))
        .toSeq
        .sorted
        .foldRight(0L)({ case (zName, acc) =>
          acc << 1 | (if (evalName(zName)) 1 else 0)
        })
    }

    def swapped(name1: String, name2: String): Circuit =
      Circuit(wireMap + (name1 -> wireMap(name2)) + (name2 -> wireMap(name1)))

    private def withInputValue(inputPrefix: String, value: Long): Circuit = {
      val (newCircuit, remainingValue) = wireMap.keys
        .filter(_.startsWith(inputPrefix))
        .toSeq
        .sorted
        .foldLeft((wireMap, value))({ case ((circuit, value), prefixName) =>
          (circuit + (prefixName -> Wire.Input((value & 1) == 1L)), value >> 1)
        })
      assert(remainingValue == 0)
      Circuit(newCircuit)
    }

    def withXValue(value: Long): Circuit = withInputValue("x", value)
    def withYValue(value: Long): Circuit = withInputValue("y", value)
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
      Circuit(inputMap ++ gateMap)
  }

  def printCircuitDot(circuit: Circuit): Unit = {
    println("digraph circuit {")
    for ((name, wire) <- circuit.wireMap) {
      wire match {
        case Wire.Input(value) =>
          println(s"  $name;")
        case Wire.Gate(lhs, op, rhs) =>
          println(s"  $name [label=\"$name $op\"];")
          println(s"  $lhs -> $name;")
          println(s"  $rhs -> $name;")
      }
    }
    println("}")
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val circuit = parseCircuit(input)
    println(circuit.zValue)
    val circuit2 = circuit.swapped("z21", "nhn").swapped("tvb", "khg").swapped("z33", "gst").swapped("z12", "vdc")
    printCircuitDot(circuit2)
    println(circuit2.zValue)
    println("51401618891888")

    val circuit3 = circuit2.withXValue(0)
    println(circuit3.zValue)

    println(Seq("z21", "nhn", "tvb", "khg", "z33", "gst", "z12", "vdc").sorted.mkString(","))
    // part 2: gst,khg,nhn,tvb,vdc,z12,z21,z33 - correct
  }
}
