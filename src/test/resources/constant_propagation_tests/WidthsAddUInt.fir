circuit WidthsAddUInt :
  module WidthsAddUInt :
    input in : UInt<3>
    output out1 : UInt<10>
    output out2 : UInt<10>

    wire temp : UInt<5> 
    temp <= add(in, UInt<1>("h0"))

    out1 <= cat(temp, temp)

    node const = add(UInt<4>("h1"), UInt<3>("h2"))
    out2 <= cat(const, const)
