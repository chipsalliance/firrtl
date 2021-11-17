// SPDX-License-Identifier: Apache-2.0

package firrtlTests.transforms

import firrtl.annotations.ReferenceTarget
import firrtl.annotations.TargetToken.{Instance, OfModule}
import firrtl.testutils.FirrtlFlatSpec
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage}
import firrtl.transforms.{CustomRadixApplyAnnotation, CustomRadixDefAnnotation}
import firrtl.util.BackendCompilationUtilities

class CustomRadixTransformSpec extends FirrtlFlatSpec {
  behavior.of("CustomRadix")

  val testDir = os.Path(BackendCompilationUtilities.createTestDirectory("CustomRadix").getAbsolutePath)
  val fir =
    """circuit M2 :
      |  module PT :
      |    input clock : Clock
      |    input reset : Reset
      |    output io : { flip i : UInt<7>, o : UInt<7>}
      |
      |    io.o <= io.i @[Playground.scala 21:8]
      |
      |  module PT_1 :
      |    input clock : Clock
      |    input reset : Reset
      |    output io : { flip i : UInt<7>, o : UInt<7>}
      |
      |    io.o <= io.i @[Playground.scala 21:8]
      |
      |  module M1 :
      |    input clock : Clock
      |    input reset : Reset
      |    output out : UInt<7>
      |
      |    reg cnt : UInt<5>, clock with :
      |      reset => (reset, UInt<5>("h0")) @[Playground.scala 28:20]
      |    node _cnt_T = add(cnt, UInt<1>("h1")) @[Playground.scala 29:14]
      |    node _cnt_T_1 = tail(_cnt_T, 1) @[Playground.scala 29:14]
      |    cnt <= _cnt_T_1 @[Playground.scala 29:7]
      |    inst pt of PT @[Playground.scala 31:18]
      |    pt.clock <= clock
      |    pt.reset <= reset
      |    inst pt2 of PT_1 @[Playground.scala 32:19]
      |    pt2.clock <= clock
      |    pt2.reset <= reset
      |    pt2.io.i <= pt.io.o @[Playground.scala 33:12]
      |    pt2.io.o is invalid @[Playground.scala 34:12]
      |    pt.io.i <= UInt<1>("h0") @[Playground.scala 36:11]
      |    node _T = eq(cnt, UInt<1>("h1")) @[Playground.scala 37:12]
      |    when _T : @[Playground.scala 37:20]
      |      pt.io.i <= UInt<1>("h1") @[Playground.scala 37:29]
      |    else :
      |      node _T_1 = eq(cnt, UInt<2>("h2")) @[Playground.scala 38:19]
      |      when _T_1 : @[Playground.scala 38:27]
      |        pt.io.i <= UInt<2>("h2") @[Playground.scala 38:36]
      |      else :
      |        node _T_2 = eq(cnt, UInt<2>("h3")) @[Playground.scala 39:19]
      |        when _T_2 : @[Playground.scala 39:27]
      |          pt.io.i <= UInt<7>("h64") @[Playground.scala 39:36]
      |        else :
      |          node _T_3 = eq(cnt, UInt<3>("h4")) @[Playground.scala 40:19]
      |          when _T_3 : @[Playground.scala 40:27]
      |            pt.io.i <= UInt<7>("h65") @[Playground.scala 40:36]
      |    out <= pt.io.o @[Playground.scala 41:7]
      |
      |  module PT_2 :
      |    input clock : Clock
      |    input reset : Reset
      |    output io : { flip i : UInt<7>, o : UInt<7>}
      |
      |    io.o <= io.i @[Playground.scala 21:8]
      |
      |  module M2 :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output out : UInt<7>
      |
      |    inst m1 of M1 @[Playground.scala 47:18]
      |    m1.clock <= clock
      |    m1.reset <= reset
      |    inst pt3 of PT_2 @[Playground.scala 48:19]
      |    pt3.clock <= clock
      |    pt3.reset <= reset
      |    pt3.io.i <= m1.out @[Playground.scala 50:12]
      |    out <= pt3.io.o @[Playground.scala 51:7]
      |""".stripMargin

  val annotations = Seq(
    FirrtlCircuitAnnotation(firrtl.Parser.parse(fir)),
    CustomRadixDefAnnotation("EnumExample", Seq(0, 1, 2, 100, 101).map(x => BigInt(x) -> s"e$x"), 7)
  ) ++ Seq(
    ("M1", Seq(), "in"),
    ("M2", Seq(Instance("pt3") -> OfModule("PT")), "io_o"),
    ("M2", Seq(Instance("m1") -> OfModule("M1"), Instance("pt2") -> OfModule("PT")), "io_i")
  ).map {
    case (module, path, ref) =>
      CustomRadixApplyAnnotation(ReferenceTarget("M2", module, path, ref, Seq()), "EnumExample")
  }

  it should "generate a JSON config file" in {
    (new FirrtlStage).execute(Array("--wave-viewer-script", "", "--target-dir", testDir.toString), annotations)
    val expected =
      """[{
        |  "EnumExample":{
        |    "width":7,
        |    "values":[{
        |      "digit":0,
        |      "alias":"e0"
        |    },{
        |      "digit":1,
        |      "alias":"e1"
        |    },{
        |      "digit":2,
        |      "alias":"e2"
        |    },{
        |      "digit":100,
        |      "alias":"e100"
        |    },{
        |      "digit":101,
        |      "alias":"e101"
        |    }],
        |    "signals":["M2.m1.pt.io_i","M2.m1.pt.io_o","M2.in"]
        |  }
        |}]""".stripMargin
    assert(expected == os.read(testDir / "custom_radix_config.json"))
  }

  it should "generate a GTKWave script file" in {
    (new FirrtlStage).execute(Array("--wave-viewer-script", "gtkwave", "--target-dir", testDir.toString), annotations)
    val expected =
      """proc signalShowString {signals fileFilter} {
        |    gtkwave::addSignalsFromList $signals
        |    gtkwave::highlightSignalsFromList $signals
        |    gtkwave::installFileFilter $fileFilter
        |    gtkwave::unhighlightSignalsFromList $signals
        |}
        |
        |set enum_EnumExample {00 e0 0000000 e0 01 e1 0000001 e1 02 e2 0000010 e2 64 e100 1100100 e100 65 e101 1100101 e101}
        |set file_EnumExample [ gtkwave::setCurrentTranslateEnums ${enum_EnumExample} ]
        |
        |signalShowString {M2.m1.pt.io_i[6:0] M2.m1.pt.io_o[6:0] M2.in[6:0]} ${file_EnumExample}
        |""".stripMargin
    assert(expected == os.read(testDir / "custom_radix_gtkwave.tcl"))
  }
}
