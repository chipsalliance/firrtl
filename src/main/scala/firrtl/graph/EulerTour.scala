package firrtl.graph

import scala.collection.mutable

/** A class that represents an Euler Tour of a directed graph from a
  * given root. This requires O(n) preprocessing time to generate the
  * initial Euler Tour.
  */
class EulerTour[T](val r: Map[T, Int], val e: Seq[T], val h: Seq[Int]) {
  private def lg(x: Double): Double = math.log(x) / math.log(2)

  /** Naive Range Minimum Query (RMQ) on an Euler Tour. This uses no
    * data structures other than the original Euler Tour and is
    * characterized by
    *
    * Performance: [Preprocessing, Query] -> [O(1), O(n)]
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
    */
  private def constructSparseTable(x: Seq[Int]): Array[Array[Int]] = {
    val tmp = Array.ofDim[Int](x.size + 1, math.ceil(lg(x.size)).toInt)
    for (i <- 0 to x.size - 1; j <- 0 to math.ceil(lg(x.size)).toInt - 1) {
      tmp(i)(j) = -1
    }

    def tableRecursive(base: Int, size: Int): Int = {
      if (size == 0) {
        tmp(base)(size) = x(base)
        x(base)
      } else {
        val (a, b, c) = (base, base + (1 << (size - 1)), size - 1)

        val l = if (tmp(a)(c) != -1) { tmp(a)(c)            }
        else                         { tableRecursive(a, c) }

        val r = if (tmp(b)(c) != -1) { tmp(b)(c)            }
        else                         { tableRecursive(b, c) }

        val min = if (l < r) l else r
        tmp(base)(size) = min
        min
      }
    }

    for (i <- (0 to x.size - 1);
      j <- (0 to math.ceil(lg(x.size)).toInt - 1);
      if i + (1 << j) - 1 < x.size + 1) yield {
      tableRecursive(i, j)
    }
    tmp
  }
  lazy val st = constructSparseTable(a)

  /** Precompute all possible RMQs for an array of size N where each
    * entry in the range is different from the last by only +-1
    */
  private def constructTableLookups(n: Int): Array[Array[Array[Int]]] = {
    val size = m - 1
    val out = Seq.fill(size)(Seq(-1, 1))
      .flatten.combinations(m - 1).flatMap(_.permutations)
      .map(_.foldLeft(Seq(0))((h, pm) => (h.head + pm) +: h).reverse)
      .map(a => {
        var tmp = Array.ofDim[Int](m, m)
        for (i <- 0 to m - 1; j <- i to m - 1) yield {
          val window = a.slice(i, j + 1)
          tmp(i)(j) = window.indexOf(window.min) + i }
        tmp }).toArray
    out
  }
  lazy val tables = constructTableLookups(m)

  /** Compute the table index of a given block.
    */
  private def mapBlockToTable(block: Seq[Int]): Int = {
    var index = 0
    var power = block.size - 2
    for (Seq(l, r) <- block.sliding(2)) {
      if (l < r) { index += 1 << power }
      power -= 1
    }
    index
  }

  /** Precompute a mapping of all blocks to table indices.
    */
  private def mapBlocksToTables(blocks: Seq[Seq[Int]]): Array[Int] = {
    val out = blocks.map(mapBlockToTable(_)).toArray
    out
  }
  lazy val tableIdx = mapBlocksToTables(blocks)

  /** Berkman--Vishkin RMQ with simplifications of
    * Bender--Farach-Colton.
    *
    * Performance: [Preprocessing, Query] -> [O(n), O(1)]
    */
  def rmqBV(x: T, y: T): T = {
    val Seq(i, j) = Seq(r(x), r(y)).sorted

    val (block_i, block_j) = (i / m, j / m)
    val (word_i,  word_j)  = (i % m, j % m)

    // Three possible scenarios:
    //   * i and j are in the same block    -> single table lookup
    //   * i and j are in adjacent blocks   -> two table lookups
    //   * i and j have blocks between them -> two table lookups, one ST lookup
    val idx = if (block_j == block_i) {
      val min_i = block_i * m + tables(tableIdx(block_i))(word_i)(word_j)
      min_i
    } else if (block_j - block_i == 1) {
      val min_i = block_i * m + tables(tableIdx(block_i))(word_i)( m - 1)
      val min_j = block_j * m + tables(tableIdx(block_j))(     0)(word_j)

      if (h(min_i) < h(min_j)) min_i else min_j
    } else {
      val min_i = block_i * m + tables(tableIdx(block_i))(word_i)( m - 1)
      val min_j = block_j * m + tables(tableIdx(block_j))(     0)(word_j)
      val (min_between_l, min_between_r) = {
        val k = math.floor(lg(block_j - block_i - 1)).toInt
        val m = block_j - (1 << k)
        val (min_0, min_1) = (h(st(block_i + 1)(k)), h(st(m)(k)))
        (min_0, min_1) }

      Seq(min_i, min_between_l, min_between_r, min_j)
        .zip(Seq(h(min_i), h(min_between_l), h(min_between_r), h(min_j)))
        .minBy(_._2)._1
    }

    e(idx)
  }

  /** Outward facing RMQ that may map to a naive or performant
    * implementation
    */
  def rmq(x: T, y: T): T = rmqBV(x, y)
}

/** Euler Tour companion object
  */
object EulerTour {
  def apply[T](diGraph: DiGraph[T], start: T): EulerTour[Seq[T]] = {
    val r = mutable.Map[Seq[T], Int]()
    val e = mutable.ArrayBuffer[Seq[T]]()
    val h = mutable.ArrayBuffer[Int]()

    def tour(u: T, parent: Seq[T] = Seq(), height: Int = 0): Unit = {
      val id = parent :+ u
      if (!r.contains(id)) { r(id) = e.size }
      e += id
      h += height
      diGraph.getEdges(id.last).toSeq.flatMap( v => {
        tour(v, id, height + 1)
        e += id
        h += height
      })
    }

    tour(start)
    new EulerTour(r.toMap, e, h)
  }
}
