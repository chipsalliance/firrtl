// See README.md for licensing info

package firrtl.graph

import scala.collection.{Set, Map}
import scala.collection.mutable
import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}

/** A companion to create DiGraphs from mutable data */
object EdgeLabelledDiGraph {
  /** Create a EdgeLabelledDiGraph from a MutableDigraph, representing the same graph */
  def apply[T, L](mdg: MutableEdgeLabelledDiGraph[T, L]): EdgeLabelledDiGraph[T, L] = mdg

  /** Create a EdgeLabelledDiGraph from a Map[T,Set[T]] of edge data */
  def apply[T, L](edgeData: Map[T,Set[(T, L)]]): EdgeLabelledDiGraph[T, L] = {
    val edgeDataCopy = new LinkedHashMap[T, LinkedHashSet[(T, L)]]
    for ((k, v) <- edgeData) {
      edgeDataCopy(k) = new LinkedHashSet[(T, L)]
    }
    for ((k, v) <- edgeData) {
      for (n <- v) {
        require(edgeDataCopy.contains(n._1), s"Does not contain ${n._1}")
        edgeDataCopy(k) += n
      }
    }
    new EdgeLabelledDiGraph(edgeDataCopy)
  }
}

/** Represents common behavior of all directed graphs */
class EdgeLabelledDiGraph[T, L] private[graph](private[graph] val edges: LinkedHashMap[T, LinkedHashSet[(T, L)]]) {
  /** Check whether the graph contains vertex v */
  def contains(v: T): Boolean = edges.contains(v)

  /** Get all vertices in the graph
    * @return a Set[T] of all vertices in the graph
    */
  // The pattern of mapping map pairs to keys maintains LinkedHashMap ordering
  def getVertices: Set[T] = new LinkedHashSet ++ edges.map({ case (k, _) => k })

  /** Get all edges of a node
    * @param v the specified node
    * @return a Set[T] of all vertices that v has edges to
    */
  def getEdges(v: T): Set[(T, L)] = edges.getOrElse(v, Set.empty)

  def getEdgeMap: Map[T, Set[(T, L)]] = edges

  /** Find all sources in the graph
    *
    * @return a Set[T] of source nodes
    */
  def findSources: Set[T] = getVertices -- edges.values.flatten.map(_._1).toSet

  /** Performs breadth-first search on the directed graph, with a blacklist of nodes
    *
    * @param root the start node
    * @param blacklist list of nodes to stop searching, if encountered
    * @return a Map[T,T] from each visited node to its predecessor in the
    * traversal
    */
  def BFS(root: T,
          labels: Seq[L],
          blacklist: Set[T],
          merge: (L, Seq[L]) => (Boolean, Seq[L])
         ): (Map[(T, Seq[L]),(T, Seq[L])], Seq[(T, Seq[L])]) = {
    val prev = new mutable.LinkedHashMap[(T, Seq[L]), (T, Seq[L])]
    val queue = new mutable.Queue[(T, Seq[L])]
    val endpoints = new mutable.ArrayBuffer[(T, Seq[L])]
    queue.enqueue((root, labels))

    while (queue.nonEmpty) {
      val (u, labels) = queue.dequeue
      if(getEdges(u).isEmpty) {
        endpoints += ((u, labels))
      } else {
        for ((v, l) <- getEdges(u)) {
          val (ok, newLabels) = merge(l, labels)
          if (ok && !prev.contains((v, newLabels)) && !blacklist.contains(v)) {
            prev((v, newLabels)) = (u, labels)
            queue.enqueue((v, newLabels))
          }
        }
      }
    }
    (prev, endpoints)
  }

  def getFullPaths(start: T,
               startingLabel: Seq[L],
               merge: (L, Seq[L]) => (Boolean, Seq[L])
              ): Seq[Seq[(T, Seq[L])]] = {

    val paths = new mutable.ArrayBuffer[Seq[(T, Seq[L])]]
    val (prev, endpoints) = BFS(start, startingLabel, Set.empty, merge)
    endpoints.foreach { case (end, endingLabel) =>
      val nodePath = new mutable.ArrayBuffer[(T, Seq[L])]
      nodePath += ((end, endingLabel))
      while (nodePath.last != start && prev.contains(nodePath.last)) {
        nodePath += prev(nodePath.last)
      }
      if (nodePath.last != start) {
        throw new PathNotFoundException
      }
      paths += nodePath.toSeq.reverse
    }
    paths
  }

  /** Finds a path (if one exists) from one node to another
    *
    * @param start the start node
    * @param end the destination node
    * @throws PathNotFoundException
    * @return a Seq[T] of nodes defining an arbitrary valid path
    */
  def path(start: T,
           startingLabel: Seq[L],
           end: T,
           endingLabel: Seq[L],
           merge: (L, Seq[L]) => (Boolean, Seq[L])
          ): Seq[(T, Seq[L])] = {
    val nodePath = new mutable.ArrayBuffer[(T, Seq[L])]
    val (prev, endpoints) = BFS(start, startingLabel, Set.empty[T], merge)
    nodePath += ((end, endingLabel))
    while (nodePath.last != start && prev.contains(nodePath.last)) {
      nodePath += prev(nodePath.last)
    }
    if (nodePath.last != start) {
      throw new PathNotFoundException
    }
    nodePath.toSeq.reverse
  }

}

class MutableEdgeLabelledDiGraph[T, L] extends EdgeLabelledDiGraph[T, L](new LinkedHashMap[T, LinkedHashSet[(T, L)]]) {
  /** Add vertex v to the graph
    * @return v, the added vertex
    */
  def addVertex(v: T): T = {
    edges.getOrElseUpdate(v, new LinkedHashSet[(T, L)])
    v
  }

  /** Add edge (u,v) to the graph.
    * @throws IllegalArgumentException if u and/or v is not in the graph
    */
  def addEdge(u: T, v: T, label: L): Unit = {
    require(contains(u))
    require(contains(v))
    edges(u) += ((v, label))
  }

  /** Add edge (u,v) to the graph, adding u and/or v if they are not
    * already in the graph.
    */
  def addPairWithEdge(u: T, v: T, label: L): Unit = {
    edges.getOrElseUpdate(v, new LinkedHashSet[(T, L)])
    edges.getOrElseUpdate(u, new LinkedHashSet[(T, L)]) += ((v, label))
  }

  /** Add edge (u,v) to the graph if and only if both u and v are in
    * the graph prior to calling addEdgeIfValid.
    */
  def addEdgeIfValid(u: T, v: T, label: L): Boolean = {
    val valid = contains(u) && contains(v)
    if (contains(u) && contains(v)) {
      edges(u) += ((v, label))
    }
    valid
  }
}

