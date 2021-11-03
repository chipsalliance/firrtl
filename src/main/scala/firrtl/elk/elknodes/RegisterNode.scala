// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

case class RegisterNode(name: String, parentOpt: Option[ElkNode]) extends ElkNode {
  override val in: String = s"$absoluteName.in"
  override val out: String = s"$absoluteName.out"
  override val asRhs: String = s"$absoluteName.out"

  def render: String = {
    val s =
      s"""
         |    node $name {
         |        layout [ size: 30, 40 ]
         |        portConstraints: FIXED_SIDE
         |        nodeLabels.placement: "H_CENTER V_TOP OUTSIDE"
         |        label "$name"
         |        label "reg" { nodeLabels.placement: "H_LEFT V_CENTER INSIDE" }
         |        port in {
         |            ^port.side: "WEST"
         |        }
         |        port out {
         |            ^port.side: "EAST"
         |        }
         |    }
         |""".stripMargin
    indentPrefix(s)
  }
}
