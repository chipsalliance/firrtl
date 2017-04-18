package firrtlTests

import java.io._
import org.scalatest._
import org.scalatest.prop._
import firrtl.Parser
import firrtl.FIRRTLEmitter
import firrtl.Circuit
import firrtl.passes.Pass

class ExampleTest extends FlatSpec with Matchers {
   val passes = Seq() //Tested passes go here
   val input =
"""circuit Top :
  module Top :
    input a : UInt<1>[2]
    node x = a
"""
   val check =
"""circuit Top : 
  module Top : 
    input a : UInt<1>[2]
    node x = a

"""
   "Any circuit" should "match exactly to its input" in {
      val cx = passes.foldLeft(Parser.parse("",input.split("\n").toIterator)) {
         (c:Circuit, p:Pass) => p.run(c)
      }
      val writer = new StringWriter()
      FIRRTLEmitter.run(cx,writer)
      val output = writer.toString
      (output == check) should be (true)
   }
}
