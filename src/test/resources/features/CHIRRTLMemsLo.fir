circuit ChirrtlMems :
  module ChirrtlMems :
    input clk : Clock
    input reset : UInt<1>
    mem ram :
      data-type => UInt<32>
      depth => 16
      read-latency => 0
      write-latency => 1
      reader => r
      writer => w
      read-under-write => undefined
    node newClock = clk
    wire wen : UInt<1>
    reg raddr : UInt<4>, clk with :
      reset => (reset, UInt<1>("h0"))
    node newerClock = clk
    reg waddr : UInt<4>, clk with :
      reset => (reset, UInt<1>("h0"))
    node GEN_0 = not(reset)
    node GEN_1 = gt(waddr, UInt<1>("h1"))
    node GEN_2 = and(GEN_0, GEN_1)
    node GEN_3 = neq(ram.r.data, raddr)
    node GEN_4 = and(GEN_2, GEN_3)
    printf(clk, GEN_4, "Assertion failed! r =/= raddr\n")
    node GEN_5 = not(reset)
    node GEN_6 = gt(waddr, UInt<1>("h1"))
    node GEN_7 = and(GEN_5, GEN_6)
    node GEN_8 = neq(ram.r.data, raddr)
    node GEN_9 = and(GEN_7, GEN_8)
    stop(clk, GEN_9, 1)
    node GEN_10 = not(reset)
    node GEN_11 = eq(raddr, UInt<4>("hf"))
    node GEN_12 = and(GEN_10, GEN_11)
    stop(clk, GEN_12, 0)
    ram.r.addr <= raddr
    ram.r.en <= UInt<1>("h1")
    ram.r.clk <= clk
    ram.w.data <= validif(wen, waddr)
    ram.w.mask <= wen
    ram.w.addr <= validif(wen, waddr)
    ram.w.en <= wen
    ram.w.clk <= validif(wen, clk)
    wen <= not(reset)
    node GEN_13 = eq(waddr, UInt<1>("h0"))
    node GEN_14 = add(raddr, UInt<1>("h1"))
    node GEN_15 = mux(GEN_13, UInt<1>("h0"), GEN_14)
    node GEN_16 = add(raddr, UInt<1>("h1"))
    node GEN_17 = mux(wen, GEN_15, GEN_16)
    raddr <= bits(GEN_17, 3, 0)
    node GEN_18 = add(waddr, UInt<1>("h1"))
    waddr <= bits(GEN_18, 3, 0)

;
; TODO: Unhandled statement Stop(@32.10,1,WRef(clk,ClockType(),PortKind(),MALE),WRef(GEN_9,UIntType(IntWidth(1)),ExpKind(),MALE))
;  TODO: Unhandled statement Stop(@34.8,0,WRef(clk,ClockType(),PortKind(),MALE),WRef(GEN_12,UIntType(IntWidth(1)),ExpKind(),MALE))
