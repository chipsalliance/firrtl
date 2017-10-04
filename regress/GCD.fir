circuit DecoupledGCD: 
  module DecoupledGCD : 
    input clock : Clock
    input reset : UInt<1>
    output io : { flip in : { flip ready : UInt<1>, valid : UInt<1>, bits : { a : UInt<32>, b : UInt<32>}}, out : { flip ready : UInt<1>, valid : UInt<1>, bits : UInt<32>}}
    io is invalid
    reg busy : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0"))
    reg done : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0"))
    reg x : UInt<32>, clock with :
      reset => (UInt<1>("h0"), x)
    reg y : UInt<32>, clock with :
      reset => (UInt<1>("h0"), y)
    node T_40 = eq(busy, UInt<1>("h0"))
    io.in.ready <= T_40
    io.out.valid <= done
    node T_42 = eq(y, UInt<1>("h0"))
    node T_43 = and(busy, T_42)
    when T_43 : 
      done <= UInt<1>("h1")
      skip
    node T_45 = and(done, io.out.ready)
    when T_45 : 
      busy <= UInt<1>("h0")
      skip
    node start = and(io.in.valid, io.in.ready)
    when start : 
      busy <= UInt<1>("h1")
      done <= UInt<1>("h0")
      skip
    node T_50 = gt(x, y)
    when T_50 : 
      node T_51 = sub(x, y)
      node T_52 = tail(T_51, 1)
      x <= T_52
      skip
    node T_54 = eq(T_50, UInt<1>("h0"))
    when T_54 : 
      node T_55 = sub(y, x)
      node T_56 = tail(T_55, 1)
      y <= T_56
      skip
    when start : 
      x <= io.in.bits.a
      y <= io.in.bits.b
      skip
    io.out.bits <= x
