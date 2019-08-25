
package firrtl
package benchmark.hot

import Utils.time

object Serialization extends App {
  val filename = args(0)
  val n = args(1).toInt
  val ext = filename.drop(filename.lastIndexOf('.'))
  val parsed = ext match {
    case ".pb" => proto.FromProto.fromFile(filename)
    case _ => Parser.parseFile(filename, Parser.UseInfo)
  }
  println("Warming up...")
  for (i <- 0 until 10) {
    val (ts, serialized) = time(parsed.serialize)
    println(f"Serialization warmup took $ts%.1f ms")
  }
  for (i <- 0 until n) {
    val (ts, serialized) = time(parsed.serialize)
    println(f"Serialization took $ts%.1f ms")
  }
}
