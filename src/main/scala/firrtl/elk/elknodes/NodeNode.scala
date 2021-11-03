// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

case class NodeNode(name: String, parentOpt: Option[ElkNode]) extends ElkNode {
  def render: String = {
    val s = s"""
               |    node $name {
               |        layout [ size: 0.01, 0.01 ]
               |    }
               |""".stripMargin
    indentPrefix(s)
  }
}
