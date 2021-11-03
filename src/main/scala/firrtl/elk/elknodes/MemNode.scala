// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

import firrtl.ir.DefMemory

import scala.collection.mutable

case class MemNode(
                    name: String,
                    parentOpt: Option[ElkNode],
                    firrtlName: String,
                    defMem: DefMemory,
                    nameToNode: mutable.HashMap[String, ElkNode])
  extends ElkNode {

  val text = new mutable.StringBuilder()

  text.append(
    s"""
       |    node $name {
       |        nodeLabels.placement: "H_CENTER V_TOP OUTSIDE"
       |        nodeSize.constraints: "NODE_LABELS"
       |        label "$name"
    """.stripMargin)

  def addPort(memPortName: String, firrtlMemPortName: String, portName: String,kind: String): Unit = {
    val firrtlName = s"$firrtlMemPortName.$portName"

    val port = MemoryPort(portName, s"$absoluteName.$memPortName.$portName", memPortName, kind)
    nameToNode(firrtlName) = port
    text.append(s"      ${port.render}")
  }

  def addMemoryPort(kind: String, memPortName: String, fields: Seq[String]): Unit = {
    val firrtlMemPortName = s"$firrtlName.$memPortName"
    val s =
      s"""
         |        node $memPortName {
         |            portConstraints: FIXED_SIDE
         |            nodeLabels.placement: "H_CENTER V_TOP OUTSIDE"
         |            nodeSize.constraints: " PORTS PORT_LABELS NODE_LABELS"
         |            label "$memPortName"
         |            label "$kind" { nodeLabels.placement: "H_CENTER V_CENTER INSIDE" }
         |""".stripMargin
    text.append(s)

    fields.foreach { portName => addPort(memPortName, firrtlMemPortName, portName, kind) }

    text.append(
      """
        |        }
    """.stripMargin)
  }

  defMem.readers.foreach { readerName =>
    addMemoryPort("read", readerName, Seq("en", "addr", "data", "clk"))
  }
  defMem.writers.foreach { readerName =>
    addMemoryPort("write", readerName, Seq("en", "addr", "data", "mask", "clk"))
  }
  defMem.readwriters.foreach { readerName =>
    addMemoryPort("write", readerName, Seq("en", "addr", "wdata", "wmask", "wmode", "clk"))
  }

  text.append(
    """
      |    }
    """.stripMargin)

  def render: String = indentPrefix(text.toString())
}

case class MemoryPort(name: String, override val absoluteName: String, memPortName: String,kind: String) extends ElkNode {
  val parentOpt: Option[ElkNode] = None // doesn't need to know parent
  def render: String = {
    val direction = if (name.endsWith("data") && kind == "read") "EAST" else "WEST"
    val s =
      s"""
         |            port $name {
         |                ^port.side: $direction
         |                label "$name"
         |            }""".stripMargin
    s
  }
}
