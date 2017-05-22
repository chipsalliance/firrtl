// See license file for details

package firrtl.transforms.clocklist

import java.io.{CharArrayWriter, Writer}

import firrtl.annotations._
import firrtl.ir._
import firrtl.transforms.clocklist.ClockListUtils._
import firrtl.transforms.core.passes.Pass
import firrtl.transforms.hierarchy.InlineInstances
import firrtl.transforms.mem.AnalysisUtils._
import firrtl.transforms.wiring.WiringUtils.{getChildrenMap, getLineage}
import firrtl.util.Utils._

/** Starting with a top module, determine the clock origins of each child instance.
 *  Write the result to writer.
 */
class ClockList(top: String, writer: Writer) extends Pass {
  def run(c: Circuit): Circuit = {
    // Build useful datastructures
    val childrenMap = getChildrenMap(c)
    val moduleMap = c.modules.foldLeft(Map[String, DefModule]())((map, m) => map + (m.name -> m))
    val lineages = getLineage(childrenMap, top)
    val outputBuffer = new CharArrayWriter

    // === Checks ===
    // TODO(izraelevitz): Check all registers/memories use "clock" clock port
    // ==============
    
    // Clock sources must be blackbox outputs and top's clock
    val partialSourceList = getSourceList(moduleMap)(lineages)
    val sourceList = partialSourceList ++ moduleMap(top).ports.collect{ case Port(i, n, Input, ClockType) => n }
    writer.append(s"Sourcelist: $sourceList \n")

    // Remove everything from the circuit, unless it has a clock type
    // This simplifies the circuit drastically so InlineInstances doesn't take forever.
    val onlyClockCircuit = RemoveAllButClocks.run(c)
    
    // Inline the clock-only circuit up to the specified top module
    val modulesToInline = (c.modules.collect { case Module(_, n, _, _) if n != top => ModuleName(n, CircuitName(c.main)) }).toSet
    val inlineTransform = new InlineInstances
    val inlinedCircuit = inlineTransform.run(onlyClockCircuit, modulesToInline, Set(), None).circuit
    val topModule = inlinedCircuit.modules.find(_.name == top).getOrElse(throwInternalError)

    // Build a hashmap of connections to use for getOrigins
    val connects = getConnects(topModule)

    // Return a map from instance name to clock origin
    val origins = getOrigins(connects, "", moduleMap)(lineages)

    // If the clock origin is contained in the source list, label good (otherwise bad)
    origins.foreach { case (instance, origin) =>
      val sep = if(instance == "") "" else "."
      if(!sourceList.contains(origin.replace('.','$'))){
        outputBuffer.append(s"Bad Origin of $instance${sep}clock is $origin\n")
      } else {
        outputBuffer.append(s"Good Origin of $instance${sep}clock is $origin\n")
      }
    }

    // Write to output file
    writer.write(outputBuffer.toString)

    // Return unchanged circuit
    c
  }
}
