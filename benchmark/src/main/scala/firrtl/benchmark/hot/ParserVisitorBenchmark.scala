// See LICENSE for license details.

package firrtl
package benchmark
package hot

import firrtl.antlr.{FIRRTLLexer, FIRRTLParser}
import org.antlr.v4.runtime.atn.PredictionMode
import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}

object ParserVisitorBenchmark extends App {
  val inputFile = args(0)
  val warmup = args(1).toInt
  val runs = args(2).toInt

  val InfoMode = Parser.IgnoreInfo
  val ctx = parse(inputFile)

  println("Benchmarking the performance of the Visitor .")

  hot.util.benchmark(warmup, runs)(new Visitor(InfoMode).visit(ctx))


  // reimplements parts of Parser.parseFile
  private def parse(filename: String): FIRRTLParser.CircuitContext = {
    val stream = CharStreams.fromFileName(filename)
    val parser = {
      val lexer = new FIRRTLLexer(stream)
      new FIRRTLParser(new CommonTokenStream(lexer))
    }

    parser.getInterpreter.setPredictionMode(PredictionMode.SLL)

    // Concrete Syntax Tree
    val cst = parser.circuit

    val numSyntaxErrors = parser.getNumberOfSyntaxErrors
    if (numSyntaxErrors > 0){
      throw new SyntaxErrorsException(s"$numSyntaxErrors syntax error(s) detected")
    }
    cst
  }
}