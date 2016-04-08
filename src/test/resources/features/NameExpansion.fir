
circuit NameExpansion :
  module OtherModule : 
    output io : { flip s : UInt<1>, flip a : UInt<4>, flip b : UInt<4>, z : UInt<4>}[2]
    output io_0_z : UInt
    io is invalid
    when io[0].s :
      io[0].z <= io[0].a
    when eq(io[0].s, UInt(0)) : 
      io[0].z <= io[0].b
      skip
    io_0_z <= io[0].z

  module NameExpansion :
    input clk : Clock
    input reset : UInt<1>

    ;wire foo : { x : UInt<4>[2], y : UInt<4>}[2]
    ;foo is invalid
    ;foo[0].x[0] <= UInt<3>("h7")
    ;foo[0].y <= UInt<4>("hd")
    ;wire foo_0_x_0 : UInt
    ;foo_0_x_0 <= foo[0].x[0]
    ;wire foo_0_y : UInt
    ;foo_0_y <= foo[0].y
    ;wire foo__0_x_0 : UInt
    ;foo__0_x_0 <= foo_0_x_0
    ;wire foo__0_y : UInt
    ;foo__0_y <= foo[0].y

    ;reg newFoo : { x : UInt<4>[2], y : UInt<4>}[2], clk with :
    ;  reset => (reset, foo) ; Test reg reset

    ;when not(reset) :
    ;  when neq(foo_0_x_0, UInt(7)) :
    ;    printf(clk, UInt(1), "Assertion failed\n")
    ;    stop(clk, UInt(1), 1)
    ;  when neq(foo__0_x_0, UInt(7)) :
    ;    printf(clk, UInt(1), "Assertion failed\n")
    ;    stop(clk, UInt(1), 1)
    ;  when neq(foo__0_y, UInt("hd")) :
    ;    printf(clk, UInt(1), "Assertion failed\n")
    ;    stop(clk, UInt(1), 1)
    ;  when neq(newFoo[0].x[0], UInt(7)) :
    ;    printf(clk, UInt(1), "Assertion failed\n")
    ;    stop(clk, UInt(1), 1)
    ;  

    ;inst mod of OtherModule
    ;mod.io is invalid
    ;mod.io[0].a <= UInt<3>("h4")
    ;mod.io[0].b <= UInt<3>("h5")
    ;mod.io[0].s <= UInt<1>("h1")
    ;wire mod_io__0_z : UInt
    ;mod_io__0_z <= mod.io[0].z
    ;node T_54 = eq(mod_io__0_z, UInt<3>("h4"))
    ;node T_55 = or(reset, T_54)
    ;node T_57 = eq(T_55, UInt<1>("h0"))
    ;when T_57 : 
    ;  printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:51 assert(mod_io_z === UInt(4))\n")
    ;  stop(clk, UInt<1>("h1"), 1)

    ;mem mem : 
    ;  data-type => { v : UInt<1>[2], info : { a : UInt<8>, b: UInt<8> }[4] }
    ;  depth => 4
    ;  read-latency => 0
    ;  write-latency => 1
    ;  reader => read
    ;  writer => write
    ;  read-under-write => undefined
    ;mem.read.clk <= clk
    ;mem.write.clk <= clk
    ;mem.write.mask.v[0] <= UInt<1>("h0")
    ;mem.write.mask.v[1] <= UInt<1>("h0")
    ;mem.write.mask.info[0].a <= UInt<1>("h0")
    ;mem.write.mask.info[1].a <= UInt<1>("h0")
    ;mem.write.mask.info[2].a <= UInt<1>("h0")
    ;mem.write.mask.info[3].a <= UInt<1>("h0")
    ;mem.write.mask.info[0].b <= UInt<1>("h0")
    ;mem.write.mask.info[1].b <= UInt<1>("h0")
    ;mem.write.mask.info[2].b <= UInt<1>("h0")
    ;mem.write.mask.info[3].b <= UInt<1>("h0")
    ;mem.write.data.info[1].a <= UInt<1>("h0")
    ;mem.write.data.info[2].a <= UInt<1>("h0")
    ;mem.write.data.info[3].a <= UInt<1>("h0")
    ;mem.write.data.info[1].b <= UInt<1>("h0")
    ;mem.write.data.info[2].b <= UInt<1>("h0")
    ;mem.write.data.info[3].b <= UInt<1>("h0")
    ;wire raddr : UInt<2>
    ;raddr <= UInt<2>("h2")
    ;wire waddr : UInt<2>
    ;waddr <= UInt<2>("h2")
    ;mem.write.addr <= waddr
    ;mem.write.en <= UInt<1>("h1")
    ;wire mem_info_0_write_data : { a : UInt<8>, b : UInt<8> }
    ;mem_info_0_write_data.a <= UInt<8>("hde")
    ;mem_info_0_write_data.b <= UInt<8>("had")
    ;mem.write.data.info[0] <= mem_info_0_write_data
    ;mem.write.data.v[0] <= UInt(1)
    ;mem.write.data.v[1] <= UInt(1)
    ;mem.write.mask.v[0] <= UInt<1>("h1")
    ;mem.write.mask.info[0].a <= UInt<1>("h1")
    ;mem.write.mask.info[0].b <= UInt<1>("h1")
    ;mem.read.addr <= raddr
    ;mem.read.en <= UInt<1>("h1")
    ;mem.read.addr is invalid
    ;node garbage = bits(mem.read.data.v[0], 0, 0)

    ;;wire mem_info_0_a_read_data : UInt
    ;;mem_info_0_a_read_data <= mem.read.data.info[0].a

    ;;node T_73 = eq(mem_info_0_a_read_data, UInt("hde"))

    ;wire woohoo_data : UInt
    ;woohoo_data <= mem.read.data.info[0].a

    ;node T_73 = eq(woohoo_data, UInt("hde"))

    ;node T_74 = or(reset, T_73)
    ;node T_76 = eq(T_74, UInt<1>("h0"))
    ;when T_76 : 
    ;  printf(clk, UInt<1>("h1"), "Assertion failed\n")
    ;  stop(clk, UInt<1>("h1"), 1)


    mem m2 : 
      data-type => UInt<4>[2]
      depth => 4
      read-latency => 0
      write-latency => 1
      reader => m2r
      writer => m2w
      read-under-write => undefined
    m2.m2r.clk <= clk
    m2.m2w.clk <= clk
    m2.m2w.mask[0] <= UInt(1)
    m2.m2w.mask[1] <= UInt(1)
    m2.m2w.data[0] <= UInt(6)
    m2.m2w.data[1] <= UInt(7)
    m2.m2w.addr <= UInt(2)
    m2.m2w.en <= UInt(1)

    m2.m2r.addr <= UInt(3)
    m2.m2r.en <= UInt(1)
    when and(not(reset), neq(m2.m2r.data[0], UInt(6))) :
      printf(clk, UInt(1), "Assertion failed\n")
      stop(clk, UInt(1), 1)
