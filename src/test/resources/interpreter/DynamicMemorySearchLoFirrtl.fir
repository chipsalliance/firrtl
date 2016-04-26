circuit DynamicMemorySearch :
  module DynamicMemorySearch :
    input clk : Clock
    input reset : UInt<1>
    input io_isWr : UInt<1>
    input io_wrAddr : UInt<3>
    input io_data : UInt<4>
    input io_en : UInt<1>
    output io_target : UInt<3>
    output io_done : UInt<1>
    reg index : UInt<3>, clk with :
      reset => (reset, UInt<3>("h0"))
    mem list :
      data-type => UInt<4>
      depth => 8
      read-latency => 0
      write-latency => 1
      reader => memVal
      writer => T_16
      writer => T_19
      writer => T_22
      writer => T_25
      writer => T_28
      writer => T_31
      writer => T_34
      writer => T_37
      writer => T_40
      writer => T_42
      read-under-write => undefined
    node T_10 = eq(io_en, UInt<1>("h0"))
    node T_11 = eq(list.memVal.data, io_data)
    node T_13 = eq(index, UInt<3>("h7"))
    node T_14 = or(T_11, T_13)
    node over = and(T_10, T_14)
    node T_44 = eq(io_isWr, UInt<1>("h0"))
    node T_45 = and(T_44, io_en)
    node T_48 = eq(over, UInt<1>("h0"))
    node T_50 = eq(io_isWr, UInt<1>("h0"))
    node T_52 = eq(io_en, UInt<1>("h0"))
    node T_53 = and(T_50, T_52)
    node T_54 = and(T_53, T_48)
    node T_56 = add(index, UInt<1>("h1"))
    node T_57 = tail(T_56, 1)
    node GEN_0 = mux(T_45, UInt<1>("h0"), index)
    index <= mux(T_54, T_57, GEN_0)
    list.memVal.addr <= index
    list.memVal.en <= UInt<1>("h1")
    list.memVal.clk <= clk
    list.T_16.data <= validif(reset, UInt<1>("h0"))
    list.T_16.mask <= reset
    list.T_16.addr <= validif(reset, UInt<1>("h0"))
    list.T_16.en <= reset
    list.T_16.clk <= validif(reset, clk)
    list.T_19.data <= validif(reset, UInt<1>("h0"))
    list.T_19.mask <= reset
    list.T_19.addr <= validif(reset, UInt<1>("h1"))
    list.T_19.en <= reset
    list.T_19.clk <= validif(reset, clk)
    list.T_22.data <= validif(reset, UInt<1>("h0"))
    list.T_22.mask <= reset
    list.T_22.addr <= validif(reset, UInt<2>("h2"))
    list.T_22.en <= reset
    list.T_22.clk <= validif(reset, clk)
    list.T_25.data <= validif(reset, UInt<1>("h0"))
    list.T_25.mask <= reset
    list.T_25.addr <= validif(reset, UInt<2>("h3"))
    list.T_25.en <= reset
    list.T_25.clk <= validif(reset, clk)
    list.T_28.data <= validif(reset, UInt<1>("h0"))
    list.T_28.mask <= reset
    list.T_28.addr <= validif(reset, UInt<3>("h4"))
    list.T_28.en <= reset
    list.T_28.clk <= validif(reset, clk)
    list.T_31.data <= validif(reset, UInt<1>("h0"))
    list.T_31.mask <= reset
    list.T_31.addr <= validif(reset, UInt<3>("h5"))
    list.T_31.en <= reset
    list.T_31.clk <= validif(reset, clk)
    list.T_34.data <= validif(reset, UInt<1>("h0"))
    list.T_34.mask <= reset
    list.T_34.addr <= validif(reset, UInt<3>("h6"))
    list.T_34.en <= reset
    list.T_34.clk <= validif(reset, clk)
    list.T_37.data <= validif(reset, UInt<1>("h0"))
    list.T_37.mask <= reset
    list.T_37.addr <= validif(reset, UInt<3>("h7"))
    list.T_37.en <= reset
    list.T_37.clk <= validif(reset, clk)
    list.T_40.data <= validif(reset, UInt<1>("h0"))
    list.T_40.mask <= reset
    list.T_40.addr <= bits(validif(reset, UInt<4>("h8")), 2, 0)
    list.T_40.en <= reset
    list.T_40.clk <= validif(reset, clk)
    list.T_42.data <= validif(io_isWr, io_data)
    list.T_42.mask <= io_isWr
    list.T_42.addr <= validif(io_isWr, io_wrAddr)
    list.T_42.en <= io_isWr
    list.T_42.clk <= validif(io_isWr, clk)
    io_target <= index
    io_done <= over
