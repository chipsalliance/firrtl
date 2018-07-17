circuit WidthsAddSInt :
  module WidthsAddSInt :
    input in : SInt<3>
    output out1 : UInt<10>
    output out2 : UInt<10>

    wire temp : SInt<5> 
    temp <= add(in, SInt<7>("h0"))

    out1 <= cat(temp, temp)

    node const = add(SInt<4>("h1"), SInt<3>("h-2"))
    out2 <= cat(const, const)
