// See LICENSE for license details.

package firrtl.graph

import scala.collection.Set

trait GraphAttribute {
  val id: String
  val attrFor: String
  val attrType: String
  val attrName: String
  val value: String

  def toGraphMLHeader =
    s"""<key id="$id" for="$attrFor" attr.name="$attrName" attr.type="$attrType"/>""".stripMargin

  // @todo: in Scala 2.13 it will be cleaner with Type tricks...
  def toGraphML =
    s"""<data key="$id">${attrType match {
      case "string" => s""""$value""""
      case _ => value
    }}</data>""".stripMargin
}

trait GraphMLEdge {
  val id: Option[String] = None
  val source: GraphMLVertex
  val target: GraphMLVertex
  val attributes: Set[GraphAttribute]
  def toGraphML =
    s"""<edge${id match {
      case Some(id) => s" $id "
      case None => " "
    }}source="${source.gmId}" target="${target.gmId}">${attributes.map(_.toGraphML).mkString("\n  ")}</edge>"""
}

trait GraphMLVertex {
  def gmId: String

  def gmAttributes: Set[GraphAttribute]

  final def gmSource = this

  def gmEdge(target: GraphMLVertex): GraphMLEdge

  def toGraphML =
    s"""<node id="${gmId}">${gmAttributes.map(_.toGraphML).mkString("\n  ")}</node>""".stripMargin
}


object GraphML {
  implicit class WithGraphML[T <: GraphMLVertex](diGraph: DiGraph[T]) {
    /* require ids to be unique. */
    require(graphMLVertices.groupBy(_.gmId).values.forall(_.size == 1))
    lazy val graphMLVertices: Set[T] = diGraph.getVertices
    lazy val graphMLEdges: Set[GraphMLEdge] = diGraph.getEdgeMap.flatMap { case (e, es) => es.map(e.gmEdge(_)) }.toSet
    lazy val graphMLAttributes: Set[GraphAttribute] = graphMLVertices.flatMap(_.gmAttributes) ++ graphMLEdges.flatMap(_.attributes)
    lazy val graphML =
      s"""<?xml version="1.0" encoding="UTF-8"?>
         |<graphml xmlns="http://graphml.graphdrawing.org/xmlns">
         |${graphMLAttributes.map(_.toGraphMLHeader).mkString("\n")}
         |<graph id="G" edgedefault="directed">
         |  ${graphMLVertices.map(_.toGraphML).mkString("\n  ")}
         |  ${graphMLEdges.map(_.toGraphML).mkString("\n  ")}
         |</graph>
         |</graphml>
      """.stripMargin
  }

  def apply[T <: GraphMLVertex](diGraph: DiGraph[T]) = diGraph.graphML
}