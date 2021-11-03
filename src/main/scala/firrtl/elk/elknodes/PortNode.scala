// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

case class PortNode(name: String, parentOpt: Option[ElkNode], isInput: Boolean = false)
    extends ElkNode {

  def render: String = {
    val direction = if (isInput) "WEST" else "EAST"
    val s = s"""
               |    port $name {
               |        ^port.side: $direction
               |        label "$name"
               |    }""".stripMargin
    indentPrefix(s)
  }
}
