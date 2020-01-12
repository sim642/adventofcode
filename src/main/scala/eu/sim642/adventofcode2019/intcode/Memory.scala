package eu.sim642.adventofcode2019.intcode

case class Memory(map: Map[Address, Value]) {

  def apply(address: Address): Value = map(address)

  def updated(address: Address, value: Value): Memory = Memory(map.updated(address, value))

  def +(addressValue: (Address, Value)): Memory = updated(addressValue._1, addressValue._2)
}

object Memory {

  def apply(values: Value*): Memory = {
    Memory(values.view.zipWithIndex.map({ case (value, i) => i -> value }).toMap.withDefaultValue(0L))
  }

  def parse(s: String): Memory = {
    Memory(s.split(',').map(_.toLong).toSeq: _*)
  }
}
