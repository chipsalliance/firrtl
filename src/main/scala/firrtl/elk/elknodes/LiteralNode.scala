// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

case class LiteralNode(name: String, value: BigInt, parentOpt: Option[ElkNode]) extends ElkNode {
  def render: String = {

    val s =
      s"""
         |    node $name {
         |        nodeLabels.placement: "H_CENTER V_CENTER INSIDE"
         |        nodeSize.constraints: "NODE_LABELS"
         |        label "$value"
         |    }
         |""".stripMargin
    indentPrefix(s)
  }
}
