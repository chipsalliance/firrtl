circuit Top :
  module Top :
    input a : UInt<32>
    output b : UInt<32>
    inst i of Inline
    i.a <= a
    b <= i.b
  module Inline :
    input a : UInt<32>
    output b : UInt<32>
    inst child of InlineChild
    child.a <= a
    b <= child.b
  module InlineChild :
    input a : UInt<32>
    output b : UInt<32>
    b <= a
