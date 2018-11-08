// See README.md for licensing info

package firrtl.graph

import scala.collection.{Set, Map}
import scala.collection.mutable
import scala.collection.mutable.{LinkedHashSet, LinkedHashMap}

/** A companion to create DiGraphs from mutable data */
object EdgeLabeledDiGraph {
  /** Create a EdgeLabeledDiGraph from a MutableDigraph, representing the same graph */
  def apply[V, L](mdg: MutableEdgeLabeledDiGraph[V, L]): EdgeLabeledDiGraph[V, L] = mdg

  /** Create a EdgeLabeledDiGraph from a Map[V,Set[V]] of edge data */
  def apply[V, L](edgeData: Map[V,Set[(V, L)]]): EdgeLabeledDiGraph[V, L] = {
    val edgeDataCopy = new LinkedHashMap[V, LinkedHashSet[(V, L)]]
    for ((k, v) <- edgeData) {
      edgeDataCopy(k) = new LinkedHashSet[(V, L)]
    }
    for ((k, v) <- edgeData) {
      for (n <- v) {
        require(edgeDataCopy.contains(n._1), s"Does not contain ${n._1}")
        edgeDataCopy(k) += n
      }
    }
    new EdgeLabeledDiGraph(edgeDataCopy)
  }
}

class TraversalState[V, L] (blacklist: Seq[V]) {
  // Current vertex
  private var vertexOpt: Option[V] = None
  def vertex: V = vertexOpt.get

  // Current state (collection of named edges so far)
  private var stateOpt: Option[Seq[L]] = None
  def state: Seq[L] = stateOpt.get

  // Map of vertex to state of traversal when visiting that vertex
  private val visited: mutable.HashMap[V, mutable.ArrayBuffer[Seq[L]]] = new mutable.HashMap()

  // Map of path so far, mapping vertex and state to previous vertex and state
  private val previous: mutable.HashMap[(V, Seq[L]), (V, Seq[L])] = mutable.HashMap.empty

  // Collection of leaf vertex
  private val leafs: mutable.HashSet[(V, Seq[L])] = mutable.HashSet.empty

  // Additional condition on whether an edge can be traversed
  private var okToTraverse: ((V, Seq[L]), (V, L)) => Boolean = { case ((vertex: V, state: Seq[L]), (to: V, via: L)) => true}
  def setTraversalCondition(condition: ((V, Seq[L]), (V, L))=>Boolean): Unit = okToTraverse = condition

  // Function to update state when traversing to a vertex
  private var updateState: ((V, Seq[L]), (V, L)) => Seq[L] = { case ((vertex: V, state: Seq[L]), (to: V, via: L)) => state :+ via }
  def setUpdateState(update: ((V, Seq[L]), (V, L))=>Seq[L]): Unit = updateState = update

  /**
    * Returns the traversal state so far
    * @return
    */
  def getState: Seq[L] = state

  /**
    * Returns the current vertex
    * @return
    */
  def getVertex: V = vertex

  def set(currentVertex: V, currentState: Seq[L]): Unit = {
    stateOpt = Some(currentState)
    vertexOpt = Some(currentVertex)
    visited.getOrElseUpdate(vertex, mutable.ArrayBuffer.empty) += state
  }

  /**
    * Returns whether we can visit the next vertex via a labeled edge
    * @param label label of edge to vertex to
    * @param to vertex to visit next
    * @return whether we can visit to via label
    */
  def canTraverse(to: V, label: L): Boolean = {
    val nextState = updateState((vertex, state), (to, label))
    !previous.contains((to, nextState)) && !blacklist.contains(to) && okToTraverse((vertex, state), (to, label))
  }

  /**
    * Update internal state, assuming we traversed to to via lable
    * @param label
    * @param to
    */
  def traverse(to: V, label: L): Unit = {

    // Calculate next State
    val nextState = updateState((vertex, state), (to, label))

    // Update our previous to next state from old state
    previous((to, nextState)) = (vertex, state)

    // Update leafs
    leafs -= ((vertex, state))
    leafs += ((to, nextState))

    // Visit current vertex/state
    visited.getOrElseUpdate(to, mutable.ArrayBuffer.empty[Seq[L]]) += nextState

    // Update vertex and state
    vertexOpt = Some(to)
    stateOpt = Some(nextState)
  }

  /**
    * Go to previous state and vertex
    */
  def backup(): Unit = {
    val (prevVertex, prevState) = previous((vertex, state))
    set(prevVertex, prevState)
  }


  /**
    * @return visited vertex/state pairs, given vertex
    */
  def getVisited: Map[V, Seq[Seq[L]]] = visited

  /**
    * @return Maps vertex/state pair to previous vertex/state pair in traversal
    */
  def getPrevious: Map[(V, Seq[L]), (V, Seq[L])] = previous

  def getLeafs: Set[(V, Seq[L])] = leafs
}

/** Represents common behavior of all directed graphs */
class EdgeLabeledDiGraph[V, L] private[graph](private[graph] val edges: LinkedHashMap[V, LinkedHashSet[(V, L)]]) {
  /** Check whether the graph contains vertex v */
  def contains(v: V): Boolean = edges.contains(v)

  /** Get all vertices in the graph
    * @return a Set[V] of all vertices in the graph
    */
  // The pattern of mapping map pairs to keys maintains LinkedHashMap ordering
  def getVertices: Set[V] = new LinkedHashSet ++ edges.map({ case (k, _) => k })

  /** Get all edges of a node
    * @param v the specified node
    * @return a Set[V] of all vertices that v has edges to
    */
  def getEdges(v: V): Set[(V, L)] = edges.getOrElse(v, Set.empty)

  def getEdgeMap: Map[V, Set[(V, L)]] = edges

  /** Find all sources in the graph
    *
    * @return a Set[V] of source nodes
    */
  def findSources: Set[V] = getVertices -- edges.values.flatten.map(_._1).toSet

  /** Performs breadth-first search on the directed graph, with a blacklist of nodes
    *
    * @param root the start node
    * @param blacklist list of nodes to stop searching, if encountered
    * @return a Map[V,V] from each visited node to its predecessor in the
    * traversal
    */
  def BFS(traversalState: TraversalState[V, L]): Unit = {
    val queue = new mutable.Queue[(V, Seq[L])]
    queue.enqueue((traversalState.getVertex, traversalState.getState))

    while (queue.nonEmpty) {
      val (u, labels) = queue.dequeue
      traversalState.set(u, labels)
      for ((v, l) <- getEdges(u)) {
        if (traversalState.canTraverse(v, l)) {
          traversalState.traverse(v, l)
          queue.enqueue((traversalState.getVertex, traversalState.getState))
          traversalState.backup()
        }
      }
    }
  }

  def getFullPaths(traversalState: TraversalState[V, L]): Seq[Seq[(V, Seq[L])]] = {

    val paths = new mutable.ArrayBuffer[Seq[(V, Seq[L])]]
    val start = traversalState.getVertex
    val startingLabel = traversalState.getState

    BFS(traversalState)

    traversalState.getLeafs.foreach { case (end, endingLabel) =>
      val nodePath = new mutable.ArrayBuffer[(V, Seq[L])]
      nodePath += ((end, endingLabel))
      while (nodePath.last != (start, startingLabel) && traversalState.getPrevious.contains(nodePath.last)) {
        nodePath += traversalState.getPrevious(nodePath.last)
      }
      if (nodePath.last != (start, startingLabel)) {
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
  def path(traversalState: TraversalState[V, L], end: (V, Seq[L])): Seq[(V, Seq[L])] = {
    val nodePath = new mutable.ArrayBuffer[(V, Seq[L])]
    val start = traversalState.getVertex
    val startingLabel = traversalState.getState
    BFS(traversalState)
    nodePath += end
    println(traversalState.getPrevious.contains(nodePath.last))
    while (nodePath.last != (start, startingLabel) && traversalState.getPrevious.contains(nodePath.last)) {
      nodePath += traversalState.getPrevious(nodePath.last)
    }
    if (nodePath.last != (start, startingLabel)) {
      println(nodePath)
      throw new PathNotFoundException
    }
    nodePath.toSeq.reverse
  }

  /** Finds a path (if one exists) from one node to another
    *
    * @param start the start node
    * @param end the destination node
    * @throws PathNotFoundException
    * @return a Seq[V] of nodes defining an arbitrary valid path
    */
  def paths(traversalState: TraversalState[V, L], end: V): Seq[Seq[(V, Seq[L])]] = {

    val start = traversalState.getVertex
    val startingState = traversalState.getState

    BFS(traversalState)

    val allpaths = mutable.ArrayBuffer[Seq[(V, Seq[L])]]()
    traversalState.getVisited(end) foreach { labels =>
      val nodePath = new mutable.ArrayBuffer[(V, Seq[L])]
      nodePath += ((end, labels))
      while (nodePath.last != start && traversalState.getPrevious.contains(nodePath.last)) {
        nodePath += traversalState.getPrevious(nodePath.last)
      }
      if (nodePath.last != (start, startingState)) {
        throw new PathNotFoundException
      }
      allpaths += nodePath.toSeq.reverse
    }
    allpaths
  }

}

class MutableEdgeLabeledDiGraph[V, L] extends EdgeLabeledDiGraph[V, L](new LinkedHashMap[V, LinkedHashSet[(V, L)]]) {
  /** Add vertex v to the graph
    * @return v, the added vertex
    */
  def addVertex(v: V): V = {
    edges.getOrElseUpdate(v, new LinkedHashSet[(V, L)])
    v
  }

  /** Add edge (u,v) to the graph.
    * @throws IllegalArgumentException if u and/or v is not in the graph
    */
  def addEdge(u: V, v: V, label: L): Unit = {
    require(contains(u))
    require(contains(v))
    edges(u) += ((v, label))
  }

  /** Add edge (u,v) to the graph, adding u and/or v if they are not
    * already in the graph.
    */
  def addPairWithEdge(u: V, v: V, label: L): Unit = {
    edges.getOrElseUpdate(v, new LinkedHashSet[(V, L)])
    edges.getOrElseUpdate(u, new LinkedHashSet[(V, L)]) += ((v, label))
  }

  /** Add edge (u,v) to the graph if and only if both u and v are in
    * the graph prior to calling addEdgeIfValid.
    */
  def addEdgeIfValid(u: V, v: V, label: L): Boolean = {
    val valid = contains(u) && contains(v)
    if (contains(u) && contains(v)) {
      edges(u) += ((v, label))
    }
    valid
  }
}

