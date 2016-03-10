; RUN: firrtl -i %s -o %s.v -X verilog -p c 2>&1 | tee %s.out | FileCheck %s
circuit Top : 
  module Top : 
    input i : UInt<1>
    input j : SInt<1>
    output i_shifted : UInt
    output j_shifted : SInt
    output k_shifted : UInt
    output l_shifted : UInt
    output m_shifted : SInt
    output n_shifted : SInt

    wire k : UInt<16>
    k <= UInt(1)
    i_shifted <= shr(i,1)
    j_shifted <= shr(j,1)
    k_shifted <= shr(k,18)
    l_shifted <= shr(UInt("hF"),4)
    m_shifted <= shr(SInt(-1),5)
    n_shifted <= shr(SInt(1),4)

;CHECK: Constant Propogation
;CHECK: circuit Top : 
;CHECK:   module Top : 
;CHECK:     input i : UInt<1>
;CHECK:     input j : SInt<1>
;CHECK:     output i_shifted : UInt<1>
;CHECK:     output j_shifted : SInt<1>
;CHECK:     output k_shifted : UInt<1>
;CHECK:     output l_shifted : UInt<1>
;CHECK:     output m_shifted : SInt<1>
;CHECK:     output n_shifted : SInt<1>
;CHECK:     wire k : UInt<16>
;CHECK:     k <= UInt<1>("h1")
;CHECK:     i_shifted <= shr(i, 1)
;CHECK:     j_shifted <= shr(j, 1)
;CHECK:     k_shifted <= shr(k, 18)
;CHECK:     l_shifted <= UInt<1>("h0")
;CHECK:     m_shifted <= SInt<1>("h1")
;CHECK:     n_shifted <= SInt<1>("h0")
