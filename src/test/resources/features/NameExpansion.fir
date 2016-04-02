
;circuit NameExpansion :
circuit OtherModule :
  module OtherModule : 
    output io : { flip s : UInt<1>, flip a : UInt<4>, flip b : UInt<4>, z : UInt<4>}
    io is invalid
    when io.s :
      io.z <= io.a
    when eq(io.s, UInt<1>("h0")) : 
      io.z <= io.b
      skip

  module NameExpansion :
    input clk : Clock
    input reset : UInt<1>

    ; Run for a few cycles
    reg count : UInt, clk with : 
      reset => (reset, UInt<3>("h0"))
    node T_82 = add(count, UInt<1>("h1"))
    node T_83 = tail(T_82, 1)
    count <= T_83
    node T_85 = eq(count, UInt<3>("h4"))
    when T_85 :
      node T_87 = eq(reset, UInt<1>("h0"))
      when T_87 : 
        stop(clk, UInt<1>("h1"), 0)

    ; registers and wires
    reg foo : { x : UInt<4>, y : UInt<4>}, clk
    foo.x <= UInt<3>("h7")
    foo.y <= UInt<4>("hd")
    wire foo_x : UInt
    foo_x <= foo.x
    reg foo_y : UInt, clk with :
      reset => (reset, foo.y)
    wire foo__x : UInt
    foo__x <= foo_x
    reg foo__y : UInt, clk with :
      reset => (reset, foo.y)

    ; invalid
    foo__y is invalid
    foo.x is invalid

    ; node and when
    node regAssertCheck = gt(count, UInt(0))
    node T_20 = eq(foo_x, UInt<3>("h7"))
    node T_21 = or(reset, T_20)
    when and(eq(T_21, UInt<1>("h0")), regAssertCheck) : 
      printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:39 assert(foo_x === UInt(7))\n")
      stop(clk, UInt<1>("h1"), 1)
    node T_27 = eq(foo_y, UInt<4>("hd"))
    node T_28 = or(reset, T_27)
    node T_30 = eq(T_28, UInt<1>("h0"))
    when T_30 : 
      printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:40 assert(foo_y === UInt(13))\n")
      stop(clk, UInt<1>("h1"), 1)
    node T_34 = eq(foo__x, UInt<3>("h7"))
    node T_35 = or(reset, T_34)
    node T_37 = eq(T_35, UInt<1>("h0"))
    when and(T_37, regAssertCheck) : 
      printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:41 assert(foo__x === UInt(7))\n")
      stop(clk, UInt<1>("h1"), 1)
    node T_41 = eq(foo__y, UInt<4>("hd"))
    node T_42 = or(reset, T_41)
    node T_44 = eq(T_42, UInt<1>("h0"))
    when T_44 : 
      printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:42 assert(foo__y === UInt(13))\n")
      stop(clk, UInt<1>("h1"), 1)

    ; instance
    inst mod of OtherModule
    mod.io.a <= UInt<3>("h4")
    mod.io.b <= UInt<3>("h5")
    mod.io.s <= UInt<1>("h1")
    wire mod_io_z : UInt
    mod_io_z <= mod.io.z
    node T_54 = eq(mod_io_z, UInt<3>("h4"))
    node T_55 = or(reset, T_54)
    node T_57 = eq(T_55, UInt<1>("h0"))
    when T_57 : 
      printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:51 assert(mod_io_z === UInt(4))\n")
      stop(clk, UInt<1>("h1"), 1)

    ; mem
    mem mem : 
      data-type => { v : UInt<1>, data : UInt<16> }
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => read
      writer => T_69
      read-under-write => undefined
    mem.read.clk <= clk
    mem.T_69.clk <= clk
    mem.T_69.mask.v <= UInt<1>("h0")
    mem.T_69.mask.data <= UInt<1>("h0")
    wire raddr : UInt<2>
    raddr <= UInt<2>("h2")
    wire waddr : UInt<2>
    waddr <= UInt<2>("h2")
    mem.T_69.addr <= waddr
    mem.T_69.en <= UInt<1>("h1")
    mem.T_69.data.data <= UInt<4>("hc")
    mem.T_69.data.v <= UInt(1)
    mem.T_69.mask.v <= UInt<1>("h1")
    mem.T_69.mask.data <= UInt<1>("h1")
    mem.read.addr <= raddr
    mem.read.en <= UInt<1>("h1")

    wire mem_read_data_data : UInt
    mem_read_data_data <= mem.read.data.data

    node T_73 = eq(mem_read_data_data, UInt<4>("hc"))
    node T_74 = or(reset, T_73)
    node T_76 = eq(T_74, UInt<1>("h0"))
    when T_76 : 
      printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:59 assert(mem(raddr) === UInt(12))\n")
      stop(clk, UInt<1>("h1"), 1)

