// SPDX-License-Identifier: Apache-2.0

package firrtl.backends.experimental.smt

private class SMTLibSpec extends SMTBackendBaseSpec {

  it should "convert a hello world module" in {
    val src =
      """circuit m:
        |  module m:
        |    input clock: Clock
        |    input a: UInt<8>
        |    output b: UInt<16>
        |    b <= a
        |    assert(clock, eq(a, b), UInt(1), "")
        |""".stripMargin

    val expected =
      """(declare-sort m_s 0)
        |; firrtl-smt2-input a 8
        |(declare-fun a_f (m_s) (_ BitVec 8))
        |; firrtl-smt2-output b 16
        |(define-fun b_f ((state m_s)) (_ BitVec 16) ((_ zero_extend 8) (a_f state)))
        |; firrtl-smt2-assert assert_ 1
        |(define-fun assert__f ((state m_s)) Bool (= ((_ zero_extend 8) (a_f state)) (b_f state)))
        |(define-fun m_t ((state m_s) (state_n m_s)) Bool true)
        |(define-fun m_i ((state m_s)) Bool true)
        |(define-fun m_a ((state m_s)) Bool (assert__f state))
        |(define-fun m_u ((state m_s)) Bool true)
        |""".stripMargin

    assert(toSMTLibStr(src) == expected)
  }

  it should "include FileInfo in the output" in {
    val src =
      """circuit m: @[circuit 0:0]
        |  module m: @[module 0:0]
        |    input clock: Clock @[clock 0:0]
        |    input a: UInt<8> @[a 0:0]
        |    output b: UInt<16> @[b 0:0]
        |    b <= a @[b_a 0:0]
        |    assert(clock, eq(a, b), UInt(1), "") @[assert 0:0]
        |""".stripMargin

    val expected =
      """; @ module 0:0
        |(declare-sort m_s 0)
        |; firrtl-smt2-input a 8
        |; @ a 0:0
        |(declare-fun a_f (m_s) (_ BitVec 8))
        |; firrtl-smt2-output b 16
        |; @ b 0:0, b_a 0:0
        |(define-fun b_f ((state m_s)) (_ BitVec 16) ((_ zero_extend 8) (a_f state)))
        |; firrtl-smt2-assert assert_ 1
        |; @ assert 0:0
        |(define-fun assert__f ((state m_s)) Bool (= ((_ zero_extend 8) (a_f state)) (b_f state)))
        |(define-fun m_t ((state m_s) (state_n m_s)) Bool true)
        |(define-fun m_i ((state m_s)) Bool true)
        |(define-fun m_a ((state m_s)) Bool (assert__f state))
        |(define-fun m_u ((state m_s)) Bool true)
        |""".stripMargin

    assert(toSMTLibStr(src) == expected)
  }
}
