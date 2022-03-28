// SPDX-License-Identifier: Apache-2.0

package firrtl.elk.elknodes

import scala.collection.mutable.ArrayBuffer

trait ElkNode {
  def render:    String
  def name:      String
  def parentOpt: Option[ElkNode]
  def absoluteName: String = {
    parentOpt match {
      case Some(parent) => s"${parent.absoluteName}.$name"
      case _            => name
    }
  }

  def in:    String = absoluteName
  def out:   String = absoluteName
  def asRhs: String = absoluteName

  val children: ArrayBuffer[ElkNode] = new ArrayBuffer[ElkNode]

  def indentPrefix(rawString: String): String = {
    val prefix = parentOpt.get.asInstanceOf[ModuleNode].indentation
    rawString.split("\n").map(prefix + _).mkString("\n")
  }
}
