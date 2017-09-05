package firrtl.graph

import scala.collection.mutable

/** A class that represents an Euler Tour of a directed graph from a
  * given root. This requires O(2n) preprocessing time to generate the
  * initiate Euler Tour.
  */
class EulerTour[T](val r: Map[T, Int], val e: Seq[T], val h: Seq[Int]) {
  private def lg(x: Double): Double = math.log(x) / math.log(2)

  /** Naive Range Minimum Query (RMQ) on an Euler Tour. This uses no
    * data structures other than the original Euler Tour and is
    * characterized by: <O(1), O(n)>
    */
  def rmqNaive(a: T, b: T): T = {
    val Seq(i, j) = Seq(r(a), r(b)).sorted
    e.zip(h).slice(i, j + 1).minBy(_._2)._1
  }

  // n: the length of the Euler cycle
  // m: the size of blocks the Euler cycle is split into
  val n = h.size
  val m = math.ceil(lg(n) / 2).toInt

  // Split up the tour, h, into blocks of size m. The last block is
  // padded to be a multiple of m. Then compute the minimum of each
  // block, a, and the index of that minimum in each block, b.
  lazy val blocks = (h ++ (1 to (m - n % m))).grouped(m).toArray
  lazy val a = blocks map (_.min)
  lazy val b = blocks map (b => b.indexOf(b.min))

  /** Construct a Sparse Table (ST) representation of an array to
    * facilitate fast RMQ queries.
    *
    * This is O(n^2 log n) but can be sped up to O(n log n) with
    * dynamic programming.
    */
  private def constructSparseTable(x: Seq[Int]): Array[Array[Int]] = {
    val tmp = Array.ofDim[Int](x.size + 1, math.ceil(lg(x.size)).toInt + 1)
    // [todo] Compute minima recursively (dynamic programming)
    for (i <- (0 to x.size);
      j <- (0 to math.ceil(lg(x.size)).toInt);
      if i + math.pow(2, j) < x.size + 1) yield {
      tmp(i)(j) = x.slice(i, i + math.pow(2, j).toInt).min }
    tmp
  }
  lazy val st = constructSparseTable(a)

  /** Precompute all possible RMQs for an array of size N where each
    * entry in the range is different from the last by only +-1
    */
  private def constructTableLookups(n: Int): Array[Array[Array[Int]]] = {
    val size = m - 1
    Seq.fill(size)(Seq(-1, 1))
      .flatten.combinations(m - 1).flatMap(_.permutations)
      .map(_.foldLeft(Seq(0))((h, pm) => (h.head + pm) +: h).reverse)
      .map(a => {
        var tmp = Array.ofDim[Int](m, m)
        for (i <- 0 to m - 1; j <- i to m - 1) yield {
          val window = a.slice(i, j + 1)
          tmp(i)(j) = window.indexOf(window.min) + i }
        tmp }).toArray
  }
  lazy val tables = constructTableLookups(m)

  /** Determine an Array-based mapping of blocks to each generated table
    */
  private def mapBlocksToTables(blocks: Seq[Seq[Int]]): Array[Int] = {
    blocks.map(b => b.map(i => i - b(0)))
      .map(b => b.sliding(2).map{ case Seq(l, r) => r > l }.toArray)
      .map(x => x.reverse.zipWithIndex.map(
        d => if (d._1) math.pow(2, d._2).toInt else 0).reduce(_ + _))
      .toArray
  }
  lazy val tableIdx = mapBlocksToTables(blocks)

  /** Berkman--Vishkin RMQ with simplifications of Bender--Farach-Colton
    */
  def rmqBV(x: T, y: T): T = {
    val Seq(i, j) = Seq(r(x), r(y)).sorted

    val (block_i, block_j) = (i / m, j / m)
    val (word_i,  word_j)  = (i % m, j % m)

    // Three possible scenarios:
    //   * i and j are in the same block    -> single table lookup
    //   * i and j are in adjacent blocks   -> two table lookups
    //   * i and j have blocks between them -> table looks and ST lookup
    val idx = if (block_j == block_i) {
      val min_i = block_i * m + tables(tableIdx(block_i))(word_i)(word_j)
      min_i
    } else if (block_j - block_i == 1) {
      val min_i = block_i * m + tables(tableIdx(block_i))(word_i)( m - 1)
      val min_j = block_j * m + tables(tableIdx(block_j))(     0)(word_j)

      Seq(min_i, min_j)
        .zip(Seq(h(min_i), h(min_j)))
        .minBy(_._2)._1
    } else {
      val min_i = block_i * m + tables(tableIdx(block_i))(word_i)( m - 1)
      val min_j = block_j * m + tables(tableIdx(block_j))(     0)(word_j)
      val min_between = {
        val k = math.floor(lg(block_j - block_i - 1)).toInt
        val m = block_j - math.pow(2, k).toInt
        val (idx_0, idx_1) = (st(block_i + 1)(k), st(m)(k))
        val (min_0, min_1) = (h(idx_0), h(idx_1))
        Seq(min_0, min_1).min }

      Seq(min_i, min_between, min_j)
        .zip(Seq(h(min_i), h(min_between), h(min_j)))
        .minBy(_._2)._1
    }

    e(idx)
  }

  def rmq(x: T, y: T): T = rmqBV(x, y)
}

object EulerTour {
  def apply[T](diGraph: DiGraph[T], start: T): EulerTour[T] = {
    val r = mutable.Map[T, Int]()
    val e = mutable.ArrayBuffer[T]()
    val h = mutable.ArrayBuffer[Int]()

    def tour(u: T, height: Int = 0): Unit = {
      if (!r.contains(u)) { r(u) = e.size }
      e += u
      h += height
      diGraph.getEdges(u).toSeq.flatMap( v => {
        tour(v, height + 1)
        e += u
        h += height
      })
    }

    tour(start)
    new EulerTour(r.toMap, e, h)
  }
}
