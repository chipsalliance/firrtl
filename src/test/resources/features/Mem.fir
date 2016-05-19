circuit Mem :
  module Mem :
    input clk : Clock
    input reset : UInt<1>

    mem memory :
      data-type => {a: UInt<16>, b: UInt<16>}
      depth => 8
      reader => r1
      reader => r2
      writer => w
      read-latency => 0
      write-latency => 1
      read-under-write => undefined
    memory.r1.clk <= clk
    memory.r1.addr is invalid
    memory.r1.en <= UInt(1)

    memory.r2.clk <= clk
    memory.r2.addr is invalid
    memory.r2.en <= UInt(1)

    memory.w.clk <= clk
    memory.w.addr is invalid
    memory.w.en <= UInt(0)
    memory.w.mask.a <= UInt(0)
    memory.w.mask.b <= UInt(0)
    memory.w.data is invalid

    reg waddr : UInt<4>, clk with : (reset => (reset, UInt(0)))
    reg wdone : UInt<1>, clk with : (reset => (reset, UInt(0)))
    waddr <= tail(add(waddr, UInt(1)), 1)

    when eq(waddr, UInt(15)) :
      wdone <= UInt(1)

    when not(wdone) :
      memory.w.en <= UInt(1)
      memory.w.mask.a <= UInt(1)
      memory.w.mask.b <= UInt(1)
      memory.w.data.a <= cat(UInt<8>("haa"), cat(waddr, waddr))
      memory.w.data.b <= cat(UInt<8>("hbb"), cat(waddr, waddr))
      

    when wdone :
      stop(clk, UInt(1), 0)
      
