
circuit NameExpansion :
  module NameExpansion :
    input clk : Clock
    input reset : UInt<1>

    reg a : { b : UInt<4>, c : UInt<4> }, clk
    a.b <= UInt(2)
    a.c <= UInt(3)

    node a_b = a.b
    node a_c = a.c

    when not(reset) :
      when neq(a_b, UInt(2)) :
        printf(clk, UInt(1), "a_b is not right!")
      when neq(a_c, UInt(3)) :
        printf(clk, UInt(1), "a_c is not right!")
      stop(clk, UInt(1), 0)

