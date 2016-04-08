circuit NameExpansionTester : 
  module OtherModule : 
    input clk : Clock
    input reset : UInt<1>
    output io : { a : { b : UInt<4>, c : { d : UInt<4>, e : UInt<4>}[2], c_0_d : UInt<4>, c__1_e : UInt<4>}, flip a_b : UInt<4>, flip a__c___0_d : UInt<4>}
    io is invalid
    wire io_a_b : UInt<4>
    io_a_b is invalid
    io_a_b <= io.a_b
    io.a.b <= io_a_b
    io.a.c[0].d <= io.a__c___0_d
  
  module NameExpansionTester : 
    input clk : Clock
    input reset : UInt<1>
    output io : { }
    io is invalid
    reg count : UInt<4>, clk with : 
      reset => (reset, UInt<4>("h0"))
    when UInt<1>("h1") : 
      node T_7 = eq(count, UInt<4>("h9"))
      node T_9 = and(UInt<1>("h1"), T_7)
      node T_12 = add(count, UInt<1>("h1"))
      node T_13 = tail(T_12, 1)
      node T_14 = mux(T_9, UInt<1>("h0"), T_13)
      count <= T_14
      skip
    node done = and(UInt<1>("h1"), T_7)
    when done : 
      node T_17 = eq(reset, UInt<1>("h0"))
      when T_17 : 
        stop(clk, UInt<1>("h1"), 0)
        skip
      skip
    reg foo : { x : UInt<4>[2], y : UInt<4>}[2], clk with : 
      reset => (UInt<1>("h0"), foo)
    foo[0].x[0] <= UInt<1>("h0")
    foo[0].x[1] <= UInt<1>("h1")
    foo[0].y <= UInt<2>("h2")
    foo[1].y <= UInt<2>("h3")
    wire foo_0_x_1 : UInt<4>
    foo_0_x_1 is invalid
    foo_0_x_1 <= foo[0].x[0]
    wire foo__1_y : UInt<4>
    foo__1_y is invalid
    foo__1_y <= foo[0].y
    reg foo__ : { x : UInt<4>[2], y : UInt<4>}[2], clk with : 
      reset => (UInt<1>("h0"), foo__)
    foo__ <= foo
    node T_150 = geq(count, UInt<1>("h1"))
    when T_150 : 
      node T_152 = eq(foo_0_x_1, UInt<1>("h0"))
      node T_154 = eq(reset, UInt<1>("h0"))
      when T_154 : 
        node T_156 = eq(T_152, UInt<1>("h0"))
        when T_156 : 
          node T_158 = eq(reset, UInt<1>("h0"))
          when T_158 : 
            printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:58 assert(foo_0_x_1 === UInt(0))\n")
            skip
          stop(clk, UInt<1>("h1"), 1)
          skip
        skip
      node T_160 = eq(foo__1_y, UInt<2>("h2"))
      node T_162 = eq(reset, UInt<1>("h0"))
      when T_162 : 
        node T_164 = eq(T_160, UInt<1>("h0"))
        when T_164 : 
          node T_166 = eq(reset, UInt<1>("h0"))
          when T_166 : 
            printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:59 assert(foo__1_y === UInt(2))\n")
            skip
          stop(clk, UInt<1>("h1"), 1)
          skip
        skip
      node T_168 = eq(foo__[0].x[1], UInt<1>("h1"))
      node T_170 = eq(reset, UInt<1>("h0"))
      when T_170 : 
        node T_172 = eq(T_168, UInt<1>("h0"))
        when T_172 : 
          node T_174 = eq(reset, UInt<1>("h0"))
          when T_174 : 
            printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:60 assert(foo__(0).x(1) === UInt(1))\n")
            skip
          stop(clk, UInt<1>("h1"), 1)
          skip
        skip
      skip
    inst mod of OtherModule
    mod.io is invalid
    mod.clk <= clk
    mod.reset <= reset
    mod.io.a_b <= UInt<3>("h4")
    mod.io.a__c___0_d <= UInt<2>("h3")
    node T_179 = eq(mod.io.a.b, UInt<3>("h4"))
    node T_181 = eq(reset, UInt<1>("h0"))
    when T_181 : 
      node T_183 = eq(T_179, UInt<1>("h0"))
      when T_183 : 
        node T_185 = eq(reset, UInt<1>("h0"))
        when T_185 : 
          printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:66 assert(mod.io.a.b === UInt(4))\n")
          skip
        stop(clk, UInt<1>("h1"), 1)
        skip
      skip
    node T_187 = eq(mod.io.a.c[0].d, UInt<3>("h4"))
    node T_189 = eq(reset, UInt<1>("h0"))
    when T_189 : 
      node T_191 = eq(T_187, UInt<1>("h0"))
      when T_191 : 
        node T_193 = eq(reset, UInt<1>("h0"))
        when T_193 : 
          printf(clk, UInt<1>("h1"), "Assertion failed\n    at Driver.scala:67 assert(mod.io.a.c(0).d === UInt(4))\n")
          skip
        stop(clk, UInt<1>("h1"), 1)
        skip
      skip

