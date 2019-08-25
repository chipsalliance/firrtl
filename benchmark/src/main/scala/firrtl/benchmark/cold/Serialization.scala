
package firrtl
package benchmark.cold

import Utils.time

object Serialization extends App {
  val filename = args(0)
  val ext = filename.drop(filename.lastIndexOf('.'))
  val parsed = ext match {
    case ".pb" => proto.FromProto.fromFile(filename)
    case _ => Parser.parseFile(filename, Parser.UseInfo)
  }
  val (ts, serialized) = time(parsed.serialize)
  println(f"Serialization took $ts%.1f ms")
}
