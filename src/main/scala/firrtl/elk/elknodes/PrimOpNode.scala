// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

object PrimOpNode {
  /* the pseudoHash is necessary because case classes with identical args have identical hashes */
  var pseudoHash: Long = 0

  def hash: Long = {
    pseudoHash += 1
    pseudoHash
  }
}

case class BinaryOpNode(
  name:         String,
  parentOpt:    Option[ElkNode],
  arg0ValueOpt: Option[String],
  arg1ValueOpt: Option[String])
    extends ElkNode {

  def in1: String = s"$absoluteName.in1"

  def in2: String = s"$absoluteName.in2"

  override val absoluteName: String = s"${name}_${PrimOpNode.hash}"
  override val asRhs:        String = s"$absoluteName.out"

  def render: String = {

    val s =
      s"""
         |    node $absoluteName {
         |        layout [ size: 32, 56 ]
         |        portConstraints: FIXED_ORDER
         |        nodeLabels.placement: "H_CENTER V_TOP OUTSIDE"
         |        portLabels.placement: "INSIDE"
         |        label "$name"
         |        port in2 {
         |            ^port.side: "WEST"${arg1ValueOpt match {
        case Some(value) => "\n" + s"""            label "$value""""
        case None        => "\n" + s"""            label "in2""""
      }}
         |        }
         |        port in1 {
         |            ^port.side: "WEST"${arg0ValueOpt match {
        case Some(value) => "\n" + s"""            label "$value""""
        case None        => "\n" + s"""            label "in1""""
      }}
         |        }
         |        port out {
         |            ^port.side: "EAST"
         |        }
         |    }
         |""".stripMargin
    indentPrefix(s)
  }
}

case class UnaryOpNode(name: String, parentOpt: Option[ElkNode]) extends ElkNode {
  def in1: String = s"$absoluteName.in1"

  override val absoluteName: String = s"${name}_${PrimOpNode.hash}"
  override val asRhs:        String = s"$absoluteName.out"

  def render: String = {
    val s =
      s"""
         |    node $absoluteName {
         |        layout [ size: 20, 40 ]
         |        portConstraints: FIXED_SIDE
         |        nodeLabels.placement: "H_CENTER V_TOP OUTSIDE"
         |        label "$name"
         |        port in1 {
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

case class OneArgOneParamOpNode(name: String, parentOpt: Option[ElkNode], value: BigInt) extends ElkNode {
  def in1: String = s"$absoluteName.in1"

  override val absoluteName: String = s"${name}_${PrimOpNode.hash}"
  override val asRhs:        String = s"$absoluteName.out"

  def render: String = {
    val s =
      s"""
         |    node $absoluteName {
         |        layout [ size: 20, 40 ]
         |        portConstraints: FIXED_SIDE
         |        nodeLabels.placement: "H_LEFT V_TOP OUTSIDE"
         |        portLabels.placement: "INSIDE"
         |        label "$name"
         |        port in1 {
         |            ^port.side: "WEST"
         |        }
         |        port in2 {
         |            ^port.side: "WEST"
         |            label "$value"
         |        }
         |        port out {
         |            ^port.side: "EAST"
         |        }
         |    }
         |""".stripMargin
    indentPrefix(s)
  }
}

case class OneArgTwoParamOpNode(
  name:      String,
  parentOpt: Option[ElkNode],
  value1:    BigInt,
  value2:    BigInt)
    extends ElkNode {
  def in1: String = s"$absoluteName.in1"

  override val absoluteName: String = s"${name}_${PrimOpNode.hash}"
  override val asRhs:        String = s"$absoluteName.out"

  def render: String = {
    val s =
      s"""
         |    node $absoluteName {
         |        layout [ size: 20, 40 ]
         |        portConstraints: FIXED_ORDER
         |        nodeLabels.placement: "H_LEFT V_TOP OUTSIDE"
         |        portLabels.placement: "INSIDE"
         |        label "$name"
         |        port in1 {
         |            ^port.side: "WEST"
         |            ^port.index:3
         |        }
         |        port in2 {
         |            ^port.side: "WEST"
         |            ^port.index:2
         |            label "$value1"
         |        }
         |        port in3 {
         |            ^port.side: "WEST"
         |            ^port.index:1
         |            label "$value2"
         |        }
         |        port out {
         |            ^port.side: "EAST"
         |        }
         |    }
         |""".stripMargin
    indentPrefix(s)
  }
}
