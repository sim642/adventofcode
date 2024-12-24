package eu.sim642.adventofcode2024

import scala.annotation.tailrec
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

  class CyclicCircuit extends RuntimeException

  case class Circuit(wireMap: Map[String, Wire]) {
    def zValue: Long = {
      val memo = mutable.Map.empty[String, Boolean]

      def evalName(name: String, called: Set[String]): Boolean =
        if (called.contains(name))
          throw new CyclicCircuit
        else
          memo.getOrElseUpdate(name, evalWire(wireMap(name), called + name))

      def evalWire(wire: Wire, called: Set[String]): Boolean = wire match {
        case Wire.Input(value) => value
        case Wire.Gate(lhs, op, rhs) =>
          val left = evalName(lhs, called)
          val right = evalName(rhs, called)
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
          acc << 1 | (if (evalName(zName, Set.empty)) 1 else 0)
        })
    }

    def dependencies(name: String): Set[String] = {
      val memo = mutable.Map.empty[String, Set[String]]

      def evalName(name: String, called: Set[String]): Set[String] =
        if (called.contains(name))
          throw new CyclicCircuit
        else
          memo.getOrElseUpdate(name, evalWire(wireMap(name), called + name) + name)

      def evalWire(wire: Wire, called: Set[String]): Set[String] = wire match {
        case Wire.Input(value) => Set.empty
        case Wire.Gate(lhs, op, rhs) =>
          val left = evalName(lhs, called)
          val right = evalName(rhs, called)
          left ++ right
      }

      evalName(name, Set.empty)
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

    def add(xValue: Long, yValue: Long): Long =
      withXValue(xValue).withYValue(yValue).zValue
  }

  def findWrongBits(circuit: Circuit): Seq[(String, String)] = {

    def isCorrect(circuit: Circuit, i: Int): Boolean = {
      (for {
        xBit <- 0 to 3
        yBit <- 0 to 3
        xValue = xBit.toLong << i >> 1
        yValue = yBit.toLong << i >> 1
        //if (try {circuit.dependencies("z45"); true} catch {case e: CyclicCircuit => false})
      } yield try {circuit.add(xValue, yValue) == xValue + yValue} catch {case e: CyclicCircuit => false}).forall(identity)
    }

    def helper(circuit: Circuit, i: Int, acc: Seq[(String, String)]): Seq[Seq[(String, String)]] = {
      if (acc.sizeIs > 4)
        Seq.empty
      else if (i > 44)
        Seq(acc)
      else if (isCorrect(circuit, i))
        helper(circuit, i + 1, acc)
      else {
        println(i)
        val depsPrev = circuit.dependencies(s"z${i - 1}")
        val deps = circuit.dependencies(s"z$i")
        val depsNext = circuit.dependencies(s"z${i + 1}")
        val depsNext2 = circuit.dependencies(s"z${i + 2}")
        val wrong1 = ((deps -- depsPrev) ++ (depsNext -- deps)).filterNot(_.startsWith("x")).filterNot(_.startsWith("y"))
        val wrong2 = (depsNext2 -- depsPrev).filterNot(_.startsWith("x")).filterNot(_.startsWith("y"))
        println(wrong1)
        println(wrong2)
        val swaps =
          for {
            name1 <- wrong1
            name2 <- wrong2
            minName = if (name1 < name2) name1 else name2
            maxName = if (name1 < name2) name2 else name1
          } yield (minName, maxName)
        for {
          (name1, name2) <- swaps.toSeq
          //name2 <- wrong2
          newCircuit = circuit.swapped(name1, name2)
          //() = println((name1, name2))
          if isCorrect(newCircuit, i - 1)
          if isCorrect(newCircuit, i)
          swap = (name1, name2)
          rest <- helper(newCircuit, i + 1, acc :+ swap)
        } yield rest
      }
    }

    val all = helper(circuit, 0, Seq.empty)
    all.foreach(println)
    all.head
  }

  def findWrongBitsString(circuit: Circuit): String =
    findWrongBits(circuit).flatMap({ case (a, b) => Seq(a, b)}).sorted.mkString(",")

  def parseInput(s: String): (String, Wire.Input) = s match {
    case s"$name: 0" => name -> Wire.Input(false)
    case s"$name: 1" => name -> Wire.Input(true)
  }

  def parseGate(s: String): (String, Wire.Gate) = s match {
    case s"$lhs AND $rhs -> $name" => name -> Wire.Gate(lhs, Op.And, rhs)
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
    findWrongBits(circuit)
    val circuit2 = circuit.swapped("z21", "nhn").swapped("tvb", "khg").swapped("z33", "gst").swapped("z12", "vdc")
    //printCircuitDot(circuit2)
    //println(circuit2.zValue)
    //println("51401618891888")

    val circuit3 = circuit2.withXValue(0)
    //println(circuit3.zValue)

    //println(Seq("z21", "nhn", "tvb", "khg", "z33", "gst", "z12", "vdc").sorted.mkString(","))
    // part 2: gst,khg,nhn,tvb,vdc,z12,z21,z33 - correct
  }
}
