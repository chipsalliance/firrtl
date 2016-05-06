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

import firrtl.{IntWidth, UIntType, NoInfo, DefMemory}
import org.scalatest.{Matchers, FlatSpec}


class MemorySpec extends FlatSpec with Matchers {

  behavior of "Memory instances"

  it should "be creatable" in {
    val dataWidth = 42
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    memory.depth should be(17)
    memory.readers.length should be(2)
    memory.readers.contains("read1") should be(true)
    memory.readers.contains("read2") should be(true)

    memory.writers.length should be(1)
    memory.writers.contains("write1") should be(true)

    memory.dataStore.length should be(17)
  }

  it should "fields of a read port can be written then read" in {
    val dataWidth = 42
    val depth = 17
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1"), Seq("write1"), Seq()
    ))

    var key = "memory1.read1.en"
//    memory.setValue(key, ConcreteUInt(Big1, dataWidth))
//    memory.getValue(key).value should be(Big1)
//    memory.setValue(key, ConcreteUInt(Big0, dataWidth))
//    memory.getValue(key).value should be(Big0)
//
//    key = "memory1.read1.addr"
//    for (value <- IntWidthTestValuesGenerator(0, depth)) {
//      memory.setValue(key, ConcreteUInt(value, memory.addressWidth))
//      memory.getValue(key).value should be(value)
//    }
    key = "memory1.read1.data"
    for (value <- IntWidthTestValuesGenerator(0, depth)) {
      memory.setValue(key, ConcreteUInt(value, memory.addressWidth))
      memory.getValue(key).value should be(value)
    }
  }

  it should "fields of a write port can be written then read" in {
    val dataWidth = 42
    val depth = 17
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1"), Seq("write1"), Seq()
    ))


    var key = "memory1.write1.en"
    memory.setValue(key, ConcreteUInt(Big1, dataWidth))
    memory.getValue(key).value should be(Big1)
    memory.setValue(key, ConcreteUInt(Big0, dataWidth))
    memory.getValue(key).value should be(Big0)

    key = "memory1.write1.addr"
    for(value <- IntWidthTestValuesGenerator(0, depth)) {
      memory.setValue(key, ConcreteUInt(value, memory.addressWidth))
      memory.getValue(key).value should be(value)
    }
    key = "memory1.write1.data"
    for(value <- IntWidthTestValuesGenerator(1, 100)) {
      memory.setValue(key, ConcreteUInt(value, dataWidth))
      memory.getValue(key).value should be(value)
    }
  }

  it should "assign to memory by setting en, data, and addr" in {
    val dataWidth = 42
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    var lastValue = Big0
    val key = "memory1.write1"
    for (i <- 0 until memory.depth) {
      println(s"Write test slot $i" + ("="*80))
      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(i, memory.addressWidth))
      memory.setValue(key + ".mask", ConcreteUInt(0, dataWidth))
      memory.setValue(key + ".data", ConcreteUInt(i * 2, dataWidth))
      memory.cycle()

      if(i > 0) memory.dataStore(i-1 % memory.depth).value should be(lastValue)
      lastValue = i * 2
    }
  }

  it should "read from memory by setting en, addr using read latency 0" in {
    val dataWidth = 42
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 0, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    val key = "memory1.read1"
    var lastValue = Big0

    for (i <- 0 until memory.depth) {
      memory.dataStore(i) = ConcreteUInt(i * 4, dataWidth)
    }

    println(s"memory is ${memory.dataStore.map(_.value).mkString(",")}")

    lastValue = 999
    val staleValue = ConcreteUInt(lastValue, dataWidth)
    memory.setValue(key + ".data", staleValue)

    for (i <- 0 until memory.depth) {
      println(s"Checking memory slot $i" + ("=" * 80))
      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(i, memory.addressWidth))
      println("enable and address set")

      memory.cycle()
      memory.getValue(key + ".data").value should be (i * 4)

      lastValue = i * 4

      println(s"got value $i ${memory.getValue(key+".data").value}")
    }
  }

  it should "read from memory by setting en, addr using read latency 1" in {
    val dataWidth = 42
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    val key = "memory1.read1"
    var lastValue = Big0

    for (i <- 0 until memory.depth) {
      memory.dataStore(i) = ConcreteUInt(i * 3, dataWidth)
    }

    println(s"memory is ${memory.dataStore.map(_.value).mkString(",")}")

    lastValue = 999
    val staleValue = ConcreteUInt(lastValue, dataWidth)
    memory.setValue(key + ".data", staleValue)
    memory.getValue(key + ".data").value should be (staleValue.value)

    for (i <- 0 until memory.depth) {
      println(s"Checking memory slot $i" + ("=" * 80))
      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(i, memory.addressWidth))
      println("enable and address set")

      println(memory)
      memory.cycle()
      println(memory)
      memory.getValue(key + ".data").value should be(i * 3)

      println(s"got value $i ${memory.getValue(key+".data").value}")
    }
  }

  it should "observe read delay" in {
    val dataWidth = 64
    for(readDelay <- 1 to 5) {
      println(s"testing read delay of $readDelay ${"="*80}")
      val memory = Memory(DefMemory(
        NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, readDelay, Seq("read1", "read2"), Seq("write1"), Seq()
      ))
      memory.setVerbose()

      val key = "memory1.read1"

      val testValue = 77
      memory.dataStore(3) = ConcreteUInt(testValue, dataWidth)
      memory.dataStore(3).value should be (testValue)

      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(3, memory.addressWidth))
      memory.setValue(key + ".data", ConcreteUInt(99, memory.dataWidth))
      memory.cycle()

      for(wait<- 0 until readDelay) {
        memory.dataStore(3).value should be (testValue)
        memory.getValue(key + ".data").value should be(99)
        memory.setValue(key + ".en", ConcreteUInt(0, 1))
        memory.cycle()
      }

      memory.dataStore(3).value should be (testValue)
      memory.getValue(key + ".data").value should be(testValue)

    }
  }

  it should "observe write delay" in {
    val dataWidth = 64
    for(writeDelay <- 1 to 5) {
      println(s"testing write delay of $writeDelay ${"="*80}")
      val memory = Memory(DefMemory(
        NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, writeDelay, 1, Seq("read1", "read2"), Seq("write1"), Seq()
      ))
      memory.setVerbose()

      val key = "memory1.write1"

      memory.dataStore(3).value should be (0)

      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(3, memory.addressWidth))
      memory.setValue(key + ".data", ConcreteUInt(11, memory.dataWidth))
      memory.cycle()

      for(wait<- 0 until writeDelay) {
        memory.dataStore(3).value should be (0)
        memory.setValue(key + ".en", ConcreteUInt(0, 1))
        memory.cycle()
      }

      memory.dataStore(3).value should be (11)

    }
  }
}
