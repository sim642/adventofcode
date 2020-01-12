package eu.sim642.adventofcode2019.intcode

case class ProgramState(memory: Memory,
                        inputs: LazyList[Value] = LazyList.empty,
                        ip: Address = 0,
                        relativeBase: Address = 0) {

  def instruction(i: Int): Value = memory(ip + i)
  def opcode: Int = (instruction(0) % 100).toInt
  def param(i: Int): Value = instruction(i + 1)
  def paramMode(i: Int): Int = ((instruction(0) / math.pow(10, 2 + i).toInt) % 10).toInt // TODO: Int pow
  def readParam(i: Int): Value = paramMode(i) match {
    case 0 => memory(param(i).toInt)
    case 1 => param(i)
    case 2 => memory((relativeBase + param(i)).toInt)
    case _ => throw new IllegalArgumentException(s"Illegal parameter read mode ${paramMode(i)}")
  }
  def writeParam(i: Int, value: Value): Memory = paramMode(i) match {
    case 0 => memory.updated(param(i).toInt, value)
    case 2 => memory.updated((relativeBase + param(i)).toInt, value)
    case _ => throw new IllegalArgumentException(s"Illegal parameter write mode ${paramMode(i)}")
  }

  def execOne: Option[(ProgramState, Option[Value])] = {
    opcode match {
      case 1 => // add
        val newValue = readParam(0) + readParam(1)
        val newMemory = writeParam(2, newValue)
        Some((copy(memory = newMemory, ip = ip + 4), None))
      case 2 => // multiply
        val newValue = readParam(0) * readParam(1)
        val newMemory = writeParam(2, newValue)
        Some((copy(memory = newMemory, ip = ip + 4), None))
      case 3 => // input
        inputs match {
          case LazyList() =>
            None // TODO: out of input indistinguishable from halt
          case input #:: newInputs =>
            val newMemory = writeParam(0, input)
            Some((copy(memory = newMemory, ip = ip + 2, inputs = newInputs), None))
        }
      case 4 => // output
        val newValue = readParam(0)
        Some((copy(ip = ip + 2), Some(newValue)))
      case 5 => // jump if true
        if (readParam(0) != 0)
          Some((copy(ip = readParam(1).toInt), None))
        else
          Some((copy(ip = ip + 3), None))
      case 6 => // jump if false
        if (readParam(0) == 0)
          Some((copy(ip = readParam(1).toInt), None))
        else
          Some((copy(ip = ip + 3), None))
      case 7 => // less than
        val newValue = if (readParam(0) < readParam(1)) 1 else 0
        val newMemory = writeParam(2, newValue)
        Some((copy(memory = newMemory, ip = ip + 4), None))
      case 8 => // equal
        val newValue = if (readParam(0) == readParam(1)) 1 else 0
        val newMemory = writeParam(2, newValue)
        Some((copy(memory = newMemory, ip = ip + 4), None))
      case 9 => // adjust relative base
        val newRelativeBase = relativeBase + readParam(0)
        Some((copy(ip = ip + 2, relativeBase = newRelativeBase.toInt), None))
      case 99 => None
      case _ => throw new IllegalArgumentException(s"Unknown opcode $opcode")
    }
  }

  def outputs: LazyList[Value] = {
    LazyList.unfold(this)(_.execOne.map(_.swap)).flatten
  }

  def execs: LazyList[(ProgramState, Option[Value])] = {
    LazyList.unfold(this)({ state =>
      state.execOne.map(ret => (ret, ret._1))
    })
  }

  def outputStates: LazyList[(ProgramState, Value)] = {
    execs.flatMap({ case (state, output) =>
      output.map((state, _))
    })
  }

  def execFinal: ProgramState = execs.last._1
}
