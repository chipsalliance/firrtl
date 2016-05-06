/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/
package firrtl.interpreter

import firrtl.{Type, Info, DefMemory}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * provides a black box implementation of a circuit memory presenting  read, write and read/write interfaces
  *
  * Assumptions:
  * Zero read_latency latches data straight to memory(address): IGNORES enable
  *
  * @param info source level information
  * @param name the name of this memory
  * @param dataType type of each memory element
  * @param depth number of elements
  * @param writeLatency how many cycles before write happens
  * @param readLatency how many cycles before read happens
  * @param readers a list of named reader ports
  * @param writers a list of named writer ports
  * @param readWriters list of named read/write ports
  * @param readUnderWrite behavior
  */
// TODO: Should enable choke if fed values other than zero or 1 or things wider than 1
class Memory(
              val info: Info,
              val name: String,
              val dataType: Type,
              val depth: Int,
              val writeLatency: Int,
              val readLatency: Int,
              val readers: Seq[String],
              val writers: Seq[String],
              val readWriters: Seq[String],
              val readUnderWrite: String
            ) extends SimpleLogger {
  import Memory._

  val dataWidth    = typeToWidth(dataType)
  val addressWidth = requiredBits(depth)
  val bigDepth     = BigInt(depth)

  assert(writeLatency < 10, s"Interpreter memory $name write latency $writeLatency not supported, must be 1")
  assert(readLatency < 10,  s"Interpreter memory $name read latency $readLatency not supported, must be 0 or 1")
  assert(readLatency >= 0,  s"Interpreter memory $name read latency $readLatency not supported, must be 0 or 1")

  val ports: Map[String, MemoryPort] = {
    (
      readers.map     { case portName => portName -> ReadPort(portName, readLatency) } ++
      writers.map     { case portName => portName -> WritePort(portName, writeLatency) } ++
      readWriters.map { case portName => portName -> ReadWritePort(portName) }
    ).toMap
  }
  val writePorts:     Array[WritePort]     = writers.map(writer => ports(writer).asInstanceOf[WritePort]).toArray
  val readPorts:      Array[ReadPort]      = readers.map(reader => ports(reader).asInstanceOf[ReadPort]).toArray
  val readWritePorts: Array[ReadWritePort] = readWriters.map(readWriter => ports(readWriter).asInstanceOf[ReadWritePort]).toArray

  val dataStore: Array[Concrete] = Array.fill(depth)(Concrete(dataType))

  def getValue(key: String): Concrete = {
    key match {
      case Memory.KeyPattern(memoryName, portName, fieldName) =>
        log(s"In memory($memoryName).port($portName).getValue($fieldName) => ${ports(portName).getValue(fieldName)})")
        ports(portName).getValue(fieldName)
      case _ =>
        throw new InterpreterException(s"Error: bad memory($key).getValue($key)")
    }
  }

  /**
    * delegate the concrete value to a port
    * various actions may ensue depending on the
    *
    * @param key full ram.port.field specifier
    * @param concreteValue current value
    */
  def setValue(key: String, concreteValue: Concrete) = {
    key match {
      case KeyPattern(memoryName, portName, fieldName) =>
        assert(name == memoryName, s"Error:bad dispatch memory($name).setValue($key, $concreteValue)")
        log(s"In memory($memoryName).port($portName).setValue($fieldName, $concreteValue)")
        ports(portName) match {
          case p: ReadPort      => p.setValue(fieldName, concreteValue)
          case p: WritePort     => p.setValue(fieldName, concreteValue)
          case p: ReadWritePort => p.setValue(fieldName, concreteValue)
        }
    }
  }

  /**
    * used to inform this memory that a cycle has passed
    */
  def cycle(): Unit = {
    for(writer <- writePorts) writer.cycle()
    for(reader <- readPorts) reader.cycle()
    for(readWriter <- readWritePorts) readWriter.cycle()

    println(s"memory($name) dataStore ${dataStore.map{_.value}.mkString(",")}")
  }

  def getFieldDependencies(portName: String): Seq[String] = {
    ports(portName).fieldDependencies
  }

  override def toString: String = {
    s"memory $name" +
    readPorts.mkString(" rp:", ",", "") +
    readPorts.mkString(" wp:", ",", "") +
      (0 until depth.min(20)).map( a => dataStore(a).value).mkString(",")
  }

  trait PipeLineElement

  abstract class MemoryPort {
    val portName: String
    val latency: Int
    var enable: Boolean     = false
    var clock: Int          = 0
    var address: Int        = 0
    var data: Concrete      = ConcreteUInt(0, dataWidth)

    def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      fieldName match {
        case "en"      => enable  = concreteValue.value > Big0
        case "clk"     => clock   = concreteValue.value.toInt
        case "addr"    => address = concreteValue.value.toInt
        case "data"    => data    = concreteValue
        case _  =>
          throw new Exception(s"error:bad field specifier memory ${fullName}.setValue($fieldName, $concreteValue)")
      }
      log(s"port is now en $enable addr $address data $data")
    }
    def getValue(fieldName: String): Concrete = {
      fieldName match {
        case "en"      => ConcreteUInt(boolToBigInt(enable), 1)
        case "clk"     => ConcreteClock(clock)
        case "addr"    => ConcreteUInt(address, addressWidth)
        case "data"    => data
        case _  =>
          throw new Exception(s"error:bad field specifier memory ${fullName}.getValue($fieldName)")
      }
    }
    def fieldDependencies: Seq[String]
    val fullName: String = s"memory ${name}.$portName"
  }

  /**
    * implements a read port with memory defined latency
    *
    * @param portName name of this reader
    * @param latency  the number of cycles between port and memory
    */
  case class ReadPort(portName: String, latency: Int) extends MemoryPort {
    val fieldDependencies = Seq("en", "addr").map { fieldName => s"$name.$portName.$fieldName"}

    case class ReadPipeLineElement(data: Concrete) {
      override def toString: String = s"[${data.value}]"
    }
    val pipeLine : ArrayBuffer[ReadPipeLineElement] = {
      ArrayBuffer.empty[ReadPipeLineElement] ++
        Array.fill(readLatency)(ReadPipeLineElement(ConcreteUInt(0, dataWidth)))
    }

    override def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      super.setValue(fieldName, concreteValue)
      inputHasChanged()
      log(s"port is now en $enable addr $address data $data")
    }
    def inputHasChanged(): Unit = {
      if(latency == 0) {
        data = dataStore(address)
      }
      else {
        pipeLine(0) = ReadPipeLineElement( if(enable) dataStore(address) else PoisonedUInt(dataWidth))
      }
    }
    def cycle(): Unit = {
      if(latency > 0) {
        data = pipeLine.remove(0).data
        pipeLine += ReadPipeLineElement(dataStore(address))
      }
    }
    override def toString: String = {
      s"${enable}:${address}:${data.value}" +
        pipeLine.mkString(" pl:", ",", "")
    }
  }

  /**
    * implements a write port with memory defined latency
    *
    * @param portName name of this writer
    * @param latency  the number of cycles between port and memory
    */
  case class WritePort(portName: String, latency: Int) extends MemoryPort {
    var mask: Concrete           = ConcreteUInt(0, dataWidth)

    case class WritePipeLineElement(enable: Boolean, address: Int, data: Concrete, mask: Concrete) {
      override def toString: String = s"[$enable:$address:${data.value}:${mask.value}]"
    }
    val pipeLine : ArrayBuffer[WritePipeLineElement] = {
      ArrayBuffer.empty[WritePipeLineElement] ++
        Array.fill(readLatency)(elementFromSnapshot)
    }

    def elementFromSnapshot = {
      WritePipeLineElement(enable, address, data, mask)
    }
    def inputHasChanged(): Unit = {
      if(latency > 0 && enable) {
        pipeLine(0) = elementFromSnapshot
      }
    }
    override def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      fieldName match {
        case "mask"    => mask = concreteValue
        case "data"    => data = concreteValue & mask.not
        case _         => super.setValue(fieldName, concreteValue)
      }
      inputHasChanged()
    }
    override def getValue(fieldName: String): Concrete = {
      fieldName match {
        case "mask"    => mask
        case _         => super.getValue(fieldName)
      }
    }
    def cycle(): Unit = {
      val element = pipeLine(0)
      if(element.enable) {
        dataStore(element.address) = {
          (dataStore(element.address) & element.mask) | (element.data & element.mask.not)
        }
      }
      pipeLine += elementFromSnapshot
    }
    val fieldDependencies = Seq("en", "addr", "data", "mask").map { fieldName => s"$name.$portName.$fieldName"}
    override def toString: String = {
      s"${enable}:${address}:${data.value},${mask.value}" +
      pipeLine.mkString(" pl:", ",", "")
    }
  }

  case class ReadWritePort(portName: String) extends MemoryPort {
    val latency = readLatency
    var writeMode: Concrete      = ConcreteUInt(0, 1)
    var readData: Concrete       = ConcreteUInt(0, dataWidth)
    var mask: Concrete           = ConcreteUInt(0, dataWidth)

    override def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      fieldName match {
        case "wmod"    => writeMode = concreteValue
        case "rdata"   => readData = concreteValue
        case "mask"    => mask = concreteValue
        case _         => super.setValue(fieldName, concreteValue)
      }
    }
    override def getValue(fieldName: String): Concrete = {
      fieldName match {
        case "wmod"    => mask
        case "rdata"   => mask
        case "mask"    => mask
        case _         => super.getValue(fieldName)
      }
    }
    def cycle(): Unit = {

    }
    val fieldDependencies = Seq("en", "addr", "mask", "wmod").map { fieldName => s"$name.$portName.$fieldName"}
    def getFieldDependencies: Seq[String] = fieldDependencies

  }
}

object Memory {
  def apply(defMemory: DefMemory): Memory = {
    new Memory(
      defMemory.info,
      defMemory.name,
      defMemory.data_type,
      defMemory.depth,
      defMemory.write_latency,
      defMemory.read_latency,
      defMemory.readers,
      defMemory.writers,
      defMemory.readwriters,
      ""
    )
  }
  def memoryKey(key: String): String = {
    key match {
      case KeyPattern(memKey, _, _) => memKey
      case _ => key
    }
  }
  val KeyPattern = """([^\.]*)\.([^\.]*)\.([^\.]*)""".r
}