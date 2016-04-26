circuit GCD :
  module GCD : 
    input clk : Clock
    input reset : UInt<1>
    output io : {flip a : UInt<32>, flip b : UInt<32>, flip e : UInt<1>, z : UInt<32>, v : UInt<1>}
    
    io is invalid
    reg x : UInt<32>, clk
    reg y : UInt<32>, clk
    node T_13 = gt(x, y)
    when T_13 :
      node T_14 = sub(x, y)
      node T_15 = tail(T_14, 1)
      x <= T_15
      skip
    node T_17 = eq(T_13, UInt<1>("h00"))
    when T_17 :
      node T_18 = sub(y, x)
      node T_19 = tail(T_18, 1)
      y <= T_19
      skip
    when io.e :
      x <= io.a
      y <= io.b
      skip
    io.z <= x
    node T_21 = eq(y, UInt<1>("h00"))
    io.v <= T_21

    
