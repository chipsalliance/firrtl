circuit AddZero :
  module AddZero :
    input in : UInt<5>
    output out1 : UInt<6>
    output out2 : UInt<6>

    out1 <= add(in, UInt<5>("h0"))
    out2 <= add(UInt<5>("h0"), in)
