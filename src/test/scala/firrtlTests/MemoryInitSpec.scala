// See LICENSE for license details.

package firrtlTests

import firrtl._
import firrtl.annotations._
import firrtl.testutils.FirrtlCheckers.containLine
import firrtl.testutils.FirrtlFlatSpec
import firrtlTests.execution._

class MemInitSpec extends FirrtlFlatSpec {
  def input(tpe: String): String =
    s"""
      |circuit MemTest:
      |  module MemTest:
      |    input clock : Clock
      |    input rAddr : UInt<5>
      |    input rEnable : UInt<1>
      |    input wAddr : UInt<5>
      |    input wData : $tpe
      |    input wEnable : UInt<1>
      |    output rData : $tpe
      |
      |    mem m:
      |      data-type => $tpe
      |      depth => 32
      |      reader => r
      |      writer => w
      |      read-latency => 1
      |      write-latency => 1
      |      read-under-write => new
      |
      |    m.r.clk <= clock
      |    m.r.addr <= rAddr
      |    m.r.en <= rEnable
      |    rData <= m.r.data
      |
      |    m.w.clk <= clock
      |    m.w.addr <= wAddr
      |    m.w.en <= wEnable
      |    m.w.data <= wData
      |    m.w.mask is invalid
      |
      |""".stripMargin

  val mRef = CircuitTarget("MemTest").module("MemTest").ref("m")
  def compile(annos: AnnotationSeq, tpe: String = "UInt<32>"): CircuitState = {
    (new VerilogCompiler).compileAndEmit(CircuitState(parse(input(tpe)), ChirrtlForm, annos))
  }

  "NoAnnotation" should "create a randomized initialization" in {
    val annos = Seq()
    val result = compile(annos)
    result should containLine ("    m[initvar] = _RAND_0[31:0];")
  }

  "InitMemoryAnnotation w/ MemoryRandomInit" should "create a randomized initialization" in {
    val annos = Seq(MemoryInitAnnotation(mRef, MemoryRandomInit()))
    val result = compile(annos)
    result should containLine ("    m[initvar] = _RAND_0[31:0];")
  }

  "InitMemoryAnnotation w/ MemoryScalarInit(0)" should "create an initialization with all zeros" in {
    val annos = Seq(MemoryInitAnnotation(mRef, MemoryScalarInit(0)))
    val result = compile(annos)
    result should containLine("    m[initvar] = 0;")
  }

  Seq(1, 3, 30, 400, 12345).foreach { value =>
    s"InitMemoryAnnotation w/ MemoryScalarInit($value)" should
      s"create an initialization with all values set to $value" in {
      val annos = Seq(MemoryInitAnnotation(mRef, MemoryScalarInit(value)))
      val result = compile(annos)
      result should containLine(s"    m[initvar] = $value;")
    }
  }

  "InitMemoryAnnotation w/ MemoryArrayInit" should "initialize all addresses" in {
    val values = Seq.tabulate(32)(ii => 2 * ii + 5).map(BigInt(_))
    val annos = Seq(MemoryInitAnnotation(mRef, MemoryArrayInit(values)))
    val result = compile(annos)
    values.zipWithIndex.foreach { case (value, addr) =>
      result should containLine(s"  m[$addr] = $value;")
    }
  }

  "InitMemoryAnnotation w/ MemoryScalarInit" should "fail for a negative value" in {
    assertThrows[EmitterException] {
      compile(Seq(MemoryInitAnnotation(mRef, MemoryScalarInit(-1))))
    }
  }

  "InitMemoryAnnotation w/ MemoryScalarInit" should "fail for a value that is too large" in {
    assertThrows[EmitterException] {
      compile(Seq(MemoryInitAnnotation(mRef, MemoryScalarInit(BigInt(1) << 32))))
    }
  }

  "InitMemoryAnnotation w/ MemoryArrayInit" should "fail for a negative value" in {
    assertThrows[EmitterException] {
      val values = Seq.tabulate(32)(_ => BigInt(-1))
      compile(Seq(MemoryInitAnnotation(mRef, MemoryArrayInit(values))))
    }
  }

  "InitMemoryAnnotation w/ MemoryArrayInit" should "fail for a value that is too large" in {
    assertThrows[EmitterException] {
      val values = Seq.tabulate(32)(_ => BigInt(1) << 32)
      compile(Seq(MemoryInitAnnotation(mRef, MemoryArrayInit(values))))
    }
  }

  "InitMemoryAnnotation w/ MemoryArrayInit" should "fail if the number of values is too small" in {
    assertThrows[EmitterException] {
      val values = Seq.tabulate(31)(_ => BigInt(1))
      compile(Seq(MemoryInitAnnotation(mRef, MemoryArrayInit(values))))
    }
  }

  "InitMemoryAnnotation w/ MemoryArrayInit" should "fail if the number of values is too large" in {
    assertThrows[EmitterException] {
      val values = Seq.tabulate(33)(_ => BigInt(1))
      compile(Seq(MemoryInitAnnotation(mRef, MemoryArrayInit(values))))
    }
  }

  "InitMemoryAnnotation on Memory with Vector type" should "fail" in {
    val caught = intercept[Exception] {
      val annos = Seq(MemoryInitAnnotation(mRef, MemoryScalarInit(0)))
      compile(annos, "UInt<32>[2]")
    }
    assert(caught.getMessage.endsWith("[module MemTest] Cannot initialize memory of non ground type UInt<32>[2]"))
  }

  "InitMemoryAnnotation on Memory with Bundle type" should "fail" in {
    val caught = intercept[Exception] {
      val annos = Seq(MemoryInitAnnotation(mRef, MemoryScalarInit(0)))
      compile(annos, "{real: SInt<10>, imag: SInt<10>}")
    }
    assert(caught.getMessage.endsWith("[module MemTest] Cannot initialize memory of non ground type { real : SInt<10>, imag : SInt<10>}"))
  }
}

abstract class MemInitExecutionSpec(values: Seq[Int], init: MemoryInitValue) extends SimpleExecutionTest with VerilogExecution {
  override val body: String =
    s"""
      |mem m:
      |  data-type => UInt<32>
      |  depth => ${values.length}
      |  reader => r
      |  read-latency => 1
      |  write-latency => 1
      |  read-under-write => new
      |m.r.clk <= clock
      |m.r.en <= UInt<1>(1)
      |""".stripMargin

  val mRef = CircuitTarget("dut").module("dut").ref("m")
  override val customAnnotations: AnnotationSeq = Seq(MemoryInitAnnotation(mRef, init))

  override def commands: Seq[SimpleTestCommand] = (Seq(-1) ++  values).zipWithIndex.map { case (value, addr) =>
    if(value == -1) Seq(Poke("m.r.addr", addr))
    else if(addr >= values.length) Seq(Expect("m.r.data", value))
    else Seq(Poke("m.r.addr", addr), Expect("m.r.data", value))
  }.flatMap(_ ++ Seq(Step(1)))
}

class MemScalarInit0ExecutionSpec extends MemInitExecutionSpec(
  Seq.tabulate(31)(_ => 0), MemoryScalarInit(0)
) {}

class MemScalarInit17ExecutionSpec extends MemInitExecutionSpec(
  Seq.tabulate(31)(_ => 17), MemoryScalarInit(17)
) {}

class MemArrayInitExecutionSpec extends MemInitExecutionSpec(
  Seq.tabulate(31)(ii => ii * 5 + 7),
  MemoryArrayInit(Seq.tabulate(31)(ii => ii * 5 + 7).map(BigInt(_)))
) {}
