// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

object MuxNode {
  var pseudoHash: Long = 0

  def hash: Long = {
    pseudoHash += 1
    pseudoHash
  }
}

case class MuxNode(
  name:         String,
  parentOpt:    Option[ElkNode],
  arg0ValueOpt: Option[String] = None,
  arg1ValueOpt: Option[String] = None)
    extends ElkNode {

  override val absoluteName: String = s"${name}_${MuxNode.hash}"
  val select:                String = s"$absoluteName.select"
  def in1:                   String = s"$absoluteName.in1"
  def in2:                   String = s"$absoluteName.in2"
  override val asRhs:        String = s"$absoluteName.out"

  def render: String = {
    val s =
      s"""
         |    node $absoluteName {
         |        layout [ size: 32, 56 ]
         |        portConstraints: FIXED_ORDER
         |        nodeLabels.placement: "H_CENTER V_TOP OUTSIDE"
         |        portLabels.placement: "INSIDE"
         |        label "mux"
         |        port select {
         |            ^port.side: "WEST"
         |            label "sel"
         |        }
         |        port in2 {
         |            ^port.side: "WEST"
         |            label "in2"
         |        }
         |        port in1 {
         |            ^port.side: "WEST"
         |            label "in1"
         |        }
         |        port out {
         |            ^port.side: "EAST"
         |        }
         |    }
         |""".stripMargin
    indentPrefix(s)
  }

}
