; RUN: firrtl -i %s -o %s.v -X verilog -p cd 2>&1 | tee %s.out; cat %s.v | FileCheck %s

circuit Top :
  module Top :
    input reset : UInt<1>
    node a = SInt("hF")

;CHECK: assign a = 4'shf;
