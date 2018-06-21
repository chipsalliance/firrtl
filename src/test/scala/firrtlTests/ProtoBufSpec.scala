// See LICENSE for license details.

package firrtlTests

import firrtl.FirrtlProtos.Firrtl
import firrtl._
import firrtl.ir._
import firrtl.Mappers._

class ProtoBufSpec extends FirrtlFlatSpec {

  /** Tests in src/test/resource/ in .fir format
    *
    * @note These tests rely on the ANTLR Parser
    */
  case class FirrtlResourceTest(name: String, resourceDir: String)

  val firrtlResourceTests = List(
    FirrtlResourceTest("GCDTester", "/integration"),
    FirrtlResourceTest("RightShiftTester", "/integration"),
    FirrtlResourceTest("MemTester", "/integration"),
    FirrtlResourceTest("PipeTester", "/integration")
    // TODO Info
    //FirrtlResourceTest("Rob", "/regress"),
    //FirrtlResourceTest("RocketCore", "/regress"),
    //FirrtlResourceTest("ICache", "/regress"),
    //FirrtlResourceTest("FPU", "/regress")
  )

  /** Helper to make circuits that are the same appear the same */
  def canonicalize(circuit: Circuit): Circuit = {
    def onModule(mod: DefModule) = mod.map(Utils.squashEmpty)

    circuit.map(onModule)
  }

  for (FirrtlResourceTest(name, dir) <- firrtlResourceTests) {
    s"$name" should "work with Protobuf serialization and deserialization" in {
      val stream = getClass.getResourceAsStream(s"$dir/$name.fir")
      val circuit = parse(scala.io.Source.fromInputStream(stream).getLines.mkString("\n"))
      val protobuf = proto.ToProto.convert(circuit)
      val circuit2 = proto.FromProto.convert(protobuf)
      canonicalize(circuit).serialize should equal(canonicalize(circuit2).serialize)
    }
  }

  "Unknown Widths" should "serialize and deserialize as unknown" in {
    import firrtl.proto._
    // Note that this has to be handled in the parent object so we need to test everything that has a width
    val uint = ir.UIntType(ir.UnknownWidth)
    FromProto.convert(ToProto.convert(uint).build) should equal (uint)

    val sint = ir.SIntType(ir.UnknownWidth)
    FromProto.convert(ToProto.convert(sint).build) should equal (sint)

    val ulit = ir.UIntLiteral(123, ir.UnknownWidth)
    FromProto.convert(ToProto.convert(ulit).build) should equal (ulit)

    val slit = ir.SIntLiteral(-123, ir.UnknownWidth)
    FromProto.convert(ToProto.convert(slit).build) should equal (slit)
  }


}
