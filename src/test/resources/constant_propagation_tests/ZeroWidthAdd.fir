circuit ZeroWidthAdd:
  module ZeroWidthAdd:
    input x: UInt<0>
    output y: UInt<7>

    node temp = add(x, UInt<9>("h0"))
    y <= cat(temp, temp)
