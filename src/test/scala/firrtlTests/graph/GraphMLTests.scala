// See LICENSE for license details.

package firrtlTests.graph

import firrtl.graph._
import firrtl.testutils._


class GraphMLTests extends FirrtlFlatSpec {
  case class SimpleNode(name: String, attributes: Set[GraphAttribute] = Set.empty) extends GraphMLVertex {
    override def id: String = name

    override def edge(target: GraphMLVertex): GraphMLEdge = SimpleEdge(this, target, {
      if(source.id == "n1" & target.id == "n3") Set(WeightAttr("2.0"))
      else if(source.id == "n5" & target.id == "n4") Set(WeightAttr("2.0"))
      else Set.empty
    })
  }

  case class SimpleEdge(source: GraphMLVertex, target: GraphMLVertex, attributes: collection.Set[GraphAttribute]) extends GraphMLEdge

  case class ColorAttr(val value: String) extends GraphAttribute {
    override val id: String = "d0"
    override val attrFor: String = "node"
    override val attrName: String = "color"
    override val attrType: String = "string"
  }

  case class WeightAttr(val value: String) extends GraphAttribute {
    override val id: String = "d1"
    override val attrFor: String = "edge"
    override val attrName: String = "weight"
    override val attrType: String = "double"
  }

  val n0 = SimpleNode("n0", Set(ColorAttr("green")))
  val n1 = SimpleNode("n1")
  val n2 = SimpleNode("n2", Set(ColorAttr("blue")))
  val n3 = SimpleNode("n3", Set(ColorAttr("red")))
  val n4 = SimpleNode("n4", Set(ColorAttr("green")))
  val n5 = SimpleNode("n5", Set(ColorAttr("turquoise")))

  val simpleGraph = DiGraph(Map(
    n0 -> Set(n1),
    n1 -> Set(n3),
    n2 -> Set(n4),
    n3 -> Set(n2, n5),
    n4 -> Set[SimpleNode](),
    n5 -> Set(n4),
  ))

  "A simple graph" should "emit graphml" in {
    val graphml = GraphML(simpleGraph)
    graphml should include("""<node id="n0"><data key="d0">"green"</data></node>""")
    graphml should include("""<node id="n1"></node>""")
    graphml should include("""<node id="n2"><data key="d0">"blue"</data></node>""")
    graphml should include("""<node id="n3"><data key="d0">"red"</data></node>""")
    graphml should include("""<node id="n4"><data key="d0">"green"</data></node>""")
    graphml should include("""<node id="n5"><data key="d0">"turquoise"</data></node>""")
    graphml should include("""<edge source="n0" target="n1"></edge>""")
    graphml should include("""<edge source="n1" target="n3"><data key="d1">2.0</data></edge>""")
    graphml should include("""<edge source="n2" target="n4"></edge>""")
    graphml should include("""<edge source="n3" target="n2"></edge>""")
    graphml should include("""<edge source="n3" target="n5"></edge>""")
    graphml should include("""<edge source="n5" target="n4"><data key="d1">2.0</data></edge>""")
    print(graphml)
  }
}
