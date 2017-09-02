package firrtl.graph

import scala.collection.immutable.{Set, Map, HashSet, HashMap}
import scala.collection.mutable
import scala.collection.mutable.MultiMap

/** Represents common behavior of all directed graphs */
trait DiGraphLike[T] {
  /** Check whether the graph contains vertex v */
  def contains(v: T): Boolean

  /** Get all vertices in the graph
    * @return a Set[T] of all vertices in the graph
    */
  def getVertices: collection.Set[T]

  /** Get all edges of a node
    * @param v the specified node
    * @return a Set[T] of all vertices that v has edges to
    */
  def getEdges(v: T): collection.Set[T]
}

/** A class to represent a mutable directed graph with nodes of type T
  * 
  * @constructor Create a new graph with the provided edge data
  * @param edges a mutable.MultiMap[T,T] of edge data
  * 
  * For the edge data MultiMap, the values associated with each vertex
  * u in the graph are the vertices with inedges from u
  */
class MutableDiGraph[T](
  private[graph] val edgeData: MultiMap[T,T] =
    new mutable.HashMap[T, mutable.Set[T]] with MultiMap[T, T]) extends DiGraphLike[T] {

  // Inherited methods from DiGraphLike
  def contains(v: T) = edgeData.contains(v)
  def getVertices = edgeData.keySet
  def getEdges(v: T) = edgeData(v)

  /** Add vertex v to the graph
    * @return v, the added vertex
    */
  def addVertex(v: T): T = {
    edgeData.getOrElseUpdate(v,new mutable.HashSet[T])
    v
  }

  /** Add edge (u,v) to the graph */
  def addEdge(u: T, v: T) = {
    // Add v to keys to maintain invariant that all vertices are keys
    // of edge data
    edgeData.getOrElseUpdate(v, new mutable.HashSet[T])
    edgeData.addBinding(u,v)
  }
}

/** A companion to create immutable DiGraphs from mutable data */
object DiGraph {
  /** Create a DiGraph from a MutableDigraph, representing the same graph */
  def apply[T](mdg: MutableDiGraph[T]): DiGraph[T] =
    new DiGraph((mdg.edgeData mapValues { _.toSet }).toMap[T, Set[T]])

  /** Create a DiGraph from a MultiMap[T] of edge data */
  def apply[T](edgeData: MultiMap[T,T]): DiGraph[T] =
    new DiGraph((edgeData mapValues { _.toSet }).toMap[T, Set[T]])

  /** Create a DiGraph from a Map[T,Set[T]] of edge data */
  def apply[T](edgeData: Map[T,Set[T]]) = new DiGraph(edgeData)
}

/**
  * A class to represent an immutable directed graph with nodes of
  * type T
  * 
  * @constructor Create a new graph with the provided edge data
  * @param edges a Map[T,Set[T]] of edge data
  * 
  * For the edge data Map, the value associated with each vertex u in
  * the graph is a Set[T] of nodes where for each node v in the set,
  * the directed edge (u,v) exists in the graph.
  */
class DiGraph[T] (val edges: Map[T, Set[T]]) extends DiGraphLike[T] {

  /** An exception that is raised when an assumed DAG has a cycle */
  class CyclicException extends Exception("No valid linearization for cyclic graph")
  /** An exception that is raised when attempting to find an unreachable node */
  class PathNotFoundException extends Exception("Unreachable node")

  // Inherited methods from DiGraphLike
  def contains(v: T) = edges.contains(v)
  def getVertices = edges.keySet
  def getEdges(v: T) = edges.getOrElse(v, new HashSet[T])

  /** Find all sources in the graph
    * 
    * @return a Set[T] of source nodes
    */
  def findSources: Set[T] = edges.keySet -- edges.values.flatten.toSet

  /** Find all sinks in the graph
    * 
    * @return a Set[T] of sink nodes
    */
  def findSinks: Set[T] = reverse.findSources

  /** Linearizes (topologically sorts) a DAG
    * 
    * @param root the start node
    * @throws CyclicException if the graph is cyclic
    * @return a Map[T,T] from each visited node to its predecessor in the
    * traversal
    */
  def linearize: Seq[T] = {
    // permanently marked nodes are implicitly held in order
    val order = new mutable.ArrayBuffer[T]
    // invariant: no intersection between unmarked and tempMarked
    val unmarked = new mutable.HashSet[T]
    val tempMarked = new mutable.HashSet[T]

    def visit(n: T): Unit = {
      if (tempMarked.contains(n)) {
        throw new CyclicException
      }
      if (unmarked.contains(n)) {
        tempMarked += n
        unmarked -= n
        for (m <- getEdges(n)) {
          visit(m)
        }
        tempMarked -= n
        order.append(n)
      }
    }

    unmarked ++= getVertices
    while (!unmarked.isEmpty) {
      visit(unmarked.head)
    }

    // visited nodes are in post-traversal order, so must be reversed
    order.reverse.toSeq
  }

  /** Performs breadth-first search on the directed graph
    * 
    * @param root the start node
    * @return a Map[T,T] from each visited node to its predecessor in the
    * traversal
    */
  def BFS(root: T): Map[T,T] = {
    val prev = new mutable.HashMap[T,T]
    val queue = new mutable.Queue[T]
    queue.enqueue(root)
    while (!queue.isEmpty) {
      val u = queue.dequeue
      for (v <- getEdges(u)) {
        if (!prev.contains(v)) {
          prev(v) = u
          queue.enqueue(v)
        }
      }
    }
    prev.toMap
  }

  /** Finds the set of nodes reachable from a particular node
    * 
    * @param root the start node
    * @return a Set[T] of nodes reachable from the root
    */
  def reachableFrom(root: T): Set[T] = BFS(root).keys.toSet

  /** Finds a path (if one exists) from one node to another
    * 
    * @param start the start node
    * @param end the destination node
    * @throws PathNotFoundException
    * @return a Seq[T] of nodes defining an arbitrary valid path
    */
  def path(start: T, end: T): Seq[T] = {
    val nodePath = new mutable.ArrayBuffer[T]
    val prev = BFS(start)
    nodePath += end
    while (nodePath.last != start && prev.contains(nodePath.last)) {
      nodePath += prev(nodePath.last)
    }
    if (nodePath.last != start) {
      throw new PathNotFoundException
    }
    nodePath.toSeq.reverse
  }

  /** Finds the strongly connected components in the graph
    * 
    * @return a Seq of Seq[T], each containing nodes of an SCC in traversable order
    */
  def findSCCs: Seq[Seq[T]] = {
    var counter: BigInt = 0
    val stack = new mutable.Stack[T]
    val onstack = new mutable.HashSet[T]
    val indices = new mutable.HashMap[T, BigInt]
    val lowlinks = new mutable.HashMap[T, BigInt]
    val sccs = new mutable.ArrayBuffer[Seq[T]]

    /*
     * Recursive code is transformed to iterative code by representing
     * call stack info in an explicit structure. Here, the stack data
     * consists of the current vertex, its currently active edge, and
     * the position in the function. Because there is only one
     * recursive call site, remembering whether a child call was
     * created on the last iteration where the current frame was
     * active is sufficient to track the position.
     */
    class StrongConnectFrame[T](val v: T, val edgeIter: Iterator[T], var childCall: Option[T] = None)
    val callStack = new mutable.Stack[StrongConnectFrame[T]]

    for (node <- getVertices) {
      callStack.push(new StrongConnectFrame(node,getEdges(node).iterator))
      while (!callStack.isEmpty) {
        val frame = callStack.top
        val v = frame.v
        frame.childCall match {
          case None =>
            indices(v) = counter
            lowlinks(v) = counter
            counter = counter + 1
            stack.push(v)
            onstack += v
          case Some(w) =>
            lowlinks(v) = lowlinks(v).min(lowlinks(w))
        }
        frame.childCall = None
        while (frame.edgeIter.hasNext && frame.childCall.isEmpty) {
          val w = frame.edgeIter.next
          if (!indices.contains(w)) {
            frame.childCall = Some(w)
            callStack.push(new StrongConnectFrame(w,getEdges(w).iterator))
          } else if (onstack.contains(w)) {
            lowlinks(v) = lowlinks(v).min(indices(w))
          }
        }
        if (frame.childCall.isEmpty) {
          if (lowlinks(v) == indices(v)) {
            val scc = new mutable.ArrayBuffer[T]
            do {
              val w = stack.pop
              onstack -= w
              scc += w
            }
            while (scc.last != v);
            sccs.append(scc.toSeq)
          }
          callStack.pop
        }
      }
    }

    sccs.toSeq
  }

  /** Finds all paths starting at a particular node in a DAG
    * 
    * WARNING: This is an exponential time algorithm (as any algorithm
    * must be for this problem), but is useful for flattening circuit
    * graph hierarchies. Each path is represented by a Seq[T] of nodes
    * in a traversable order.
    * 
    * @param start the node to start at
    * @return a Map[T,Seq[Seq[T]]] where the value associated with v is the Seq of all paths from start to v
    */
  def pathsInDAG(start: T): Map[T,Seq[Seq[T]]] = {
    // paths(v) holds the set of paths from start to v
    val paths = new mutable.HashMap[T,mutable.Set[Seq[T]]] with mutable.MultiMap[T,Seq[T]]
    val queue = new mutable.Queue[T]
    val reachable = reachableFrom(start)
    paths.addBinding(start,Seq(start))
    queue += start
    queue ++= linearize.filter(reachable.contains(_))
    while (!queue.isEmpty) {
      val current = queue.dequeue
      for (v <- getEdges(current)) {
        for (p <- paths(current)) {
          paths.addBinding(v, p :+ v)
        }
      }
    }
      (paths map { case (k,v) => (k,v.toSeq) }).toMap
  }

  /** Returns a graph with all edges reversed */
  def reverse: DiGraph[T] = {
    val mdg = new MutableDiGraph[T]
    edges.foreach { case (u, edges) =>
      mdg.addVertex(u)
      edges.foreach(v => mdg.addEdge(v,u))
    }
    DiGraph(mdg)
  }

  /** Return a graph with only a subset of the nodes
    *
    * Any edge including a deleted node will be deleted
    * 
    * @param vprime the Set[T] of desired vertices
    * @throws IllegalArgumentException if vprime is not a subset of V
    * @return the subgraph
    */
  def subgraph(vprime: Set[T]): DiGraph[T] = {
    require(vprime.subsetOf(edges.keySet))
    val eprime = vprime.map(v => (v,getEdges(v) & vprime)).toMap
    new DiGraph(eprime)
  }

  /** Return a graph with only a subset of the nodes
    *
    * Any path between two non-deleted nodes (u,v) that traverses only
    * deleted nodes will be transformed into an edge (u,v).
    * 
    * @param vprime the Set[T] of desired vertices
    * @throws IllegalArgumentException if vprime is not a subset of V
    * @return the simplified graph
    */
  def simplify(vprime: Set[T]): DiGraph[T] = {
    require(vprime.subsetOf(edges.keySet))
    val eprime = vprime.map( v => (v,reachableFrom(v) & (vprime-v)) ).toMap
    new DiGraph(eprime)
  }

  /** Return a graph with all the nodes of the current graph transformed
    * by a function. Edge connectivity will be the same as the current
    * graph.
    * 
    * @param f A function {(T) => Q} that transforms each node
    * @return a transformed DiGraph[Q]
    */
  def transformNodes[Q](f: (T) => Q): DiGraph[Q] = {
    val eprime = edges.map({ case (k,v) => (f(k),v.map(f(_))) })
    new DiGraph(eprime)
  }

  /** Case class representing an Euler Tour of a tree
    */
  case class EulerTour(r: Map[T, Int], e: Seq[T], h: Seq[Int])

  /** Return an Euler Tour of a tree from a root
    *
    *  @return An Euler Tour
    */
  def eulerTour(start: T): EulerTour = {
    val r = mutable.Map[T, Int]()
    val e = mutable.ArrayBuffer[T]()
    val h = mutable.ArrayBuffer[Int]()

    def tour(u: T, height: Int = 0): Unit = {
      if (!r.contains(u)) { r(u) = e.size }
      e += u
      h += height
      getEdges(u).toSeq.flatMap( v => {
        tour(v, height + 1)
        e += u
        h += height
      })
    }

    tour(start)
    EulerTour(r.toMap, e, h)
  }

  /** Naive Range Minimum Query (RMQ) on an Euler Tour
    */
  def naiveRmq(t: EulerTour, a: T, b: T): T = {
    val Seq(l, r) = Seq(t.r(a), t.r(b)).sorted
    t.e.zip(t.h).slice(l, r + 1).minBy(_._2)._1
  }

  /** Berkman--Vishkin RMQ with simplifications of Bender--Farach-Colton
    */
  def pmOneRmq(t: EulerTour, x: T, y: T): T = {
    val Seq(i, j) = Seq(t.r(x), t.r(y)).sorted
    def lg(x: Double): Double = math.log(x) / math.log(2)
    val n = math.ceil(lg(t.h.size) / 2).toInt

    // Split up the tour (t.h) into blocks of size n. The last block
    // is padded to be a multiple of n. Then compute (a) the minimum
    // of each block and (b) the index of that minimum in each block.
    val blocks = (t.h ++ (1 to (n - t.h.size % n))).grouped(n).toArray
    val (a, b) = (blocks.map(_.min), blocks.map(b => b.indexOf(b.min)))

    /** Construct a Sparse Table (ST) representation of an array to
      * facilitate fast RMQ queries.
      *
      * This is O(n^2 log n) but can be sped up to O(n log n) with
      * dynamic programming.
      */
    def constructSparseTable(x: Seq[Int]): Array[Array[Int]] = {
      val tmp = Array.ofDim[Int](x.size + 1, math.ceil(lg(x.size)).toInt + 1)
      // [todo] Compute minima recursively (dynamic programming)
      for (i <- (0 to x.size);
        j <- (0 to math.ceil(lg(x.size)).toInt);
        if i + math.pow(2, j) < x.size + 1) yield {
          tmp(i)(j) = x.slice(i, i + math.pow(2, j).toInt).min }
      tmp
    }
    val st = constructSparseTable(a)

    /** Precompute all possible RMQs for an array of size N where each
      * entry in the range is different from the last by only +-1
      */
    def constructTableLookups(n: Int): Array[Array[Array[Int]]] = {
      val size = n - 1
      Seq.fill(size)(Seq(-1, 1))
        .flatten.combinations(n - 1).flatMap(_.permutations)
        .map(_.foldLeft(Seq(0))((h, pm) => (h.head + pm) +: h).reverse)
        .map(a => {
          var tmp = Array.ofDim[Int](n, n)
          for (i <- 0 to n - 1; j <- i to n - 1) yield {
            val window = a.slice(i, j + 1)
            tmp(i)(j) = window.indexOf(window.min) + i }
          tmp }).toArray
    }
    val tables = constructTableLookups(n)

    // Determine the table index of each block
    val tableIdx = blocks.map(b => b.map(i => i - b(0)))
      .map(b => b.sliding(2).map{ case Seq(l, r) => r > l }.toArray)
      .map(x => x.reverse.zipWithIndex.map(
        d => if (d._1) math.pow(2, d._2).toInt else 0).reduce(_ + _))

    val Seq(block_i, block_j) = Seq(i, j) map (_ / n)
    val Seq(word_i,  word_j)  = Seq(i, j) map (_ % n)

    val idx = if (block_j == block_i) {
      block_i * n + tables(tableIdx(block_i))(word_i)(word_j)
    } else if (block_j - block_i == 1) {
      val min_i = block_i * n + tables(tableIdx(block_i))(word_i)(n - 1)
      val min_j = block_j * n + tables(tableIdx(block_j))(0)(word_j)

      Seq(min_i, min_j)
        .zip(Seq(t.h(min_i), t.h(min_j)))
        .minBy(_._2)._1
    } else {
      val min_i = block_i * n + tables(tableIdx(block_i))(word_i)(n - 1)
      val min_j = block_j * n + tables(tableIdx(block_j))(0)(word_j)
      val min_between = {
        val k = math.floor(lg(block_j - block_i - 1)).toInt
        val n = block_j - math.pow(2, k).toInt
        val (idx_0, idx_1) = (st(block_i + 1)(k), st(n)(k))
        val (min_0, min_1) = (t.h(idx_0), t.h(idx_1))
        Seq(min_0, min_1).min }

      Seq(min_i, min_between, min_j)
        .zip(Seq(t.h(min_i), t.h(min_between), t.h(min_j)))
        .minBy(_._2)._1
    }

    t.e(idx)
  }
}
