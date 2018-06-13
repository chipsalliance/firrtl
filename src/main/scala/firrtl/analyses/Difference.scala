// See LICENSE for license details.

package firrtl.analyses

import firrtl.CircuitState
import firrtl.ir._
import firrtl.Mappers._

import scala.collection.mutable

import Difference._

object Difference {
  def vertexEquals(b: FirrtlNode, a: FirrtlNode): Boolean = (b, a) match {
    case (b: Product, a: Product) =>
      val bKids = b.productIterator.toSeq
      val aKids = a.productIterator.toSeq
      val sameSize = bKids.size == aKids.size
      sameSize && getOtherKids(b).zip(getOtherKids(a)).forall{ case (x, y) => x == y}
    case _ => false
  }
  def getString(f: FirrtlNode): String = f match {
    case p: Product =>
      p.getClass().getSimpleName() + "(" +
        p.productIterator.map(x => x match {
          case _: Expression|_: Statement|_: Type|_: DefModule|_: Circuit|_: Port =>
            x.getClass().getSimpleName()
          case _: Seq[_] =>
            "Seq"
          case _ => x.toString()
        }).mkString(", ") + ")"
    case _ => ???
  }
  def getOtherKids(f: FirrtlNode): Seq[Any] = f match {
    case p: Product =>
      p.productIterator.filter(_ match {
        case _: Expression|_: Statement|_: Type|_: DefModule|_: Circuit|_: Port |_: Seq[_]=> false
        case _ => true
      }).toSeq
    case _ => ???
  }
  /** Returns children FirrtlNodes */
  def getKids(f: FirrtlNode): Seq[FirrtlNode] = {
    val kids = mutable.ArrayBuffer[FirrtlNode]()
    def addMKid(m: DefModule): DefModule = { kids += m; m }
    def addPKid(p: DefPort): DefPort = { kids += p; p }
    def addSKid(s: Statement): Statement = { kids += s; s }
    def addEKid(e: Expression): Expression = { kids += e; e }
    def addTKid(t: Type): Type = { kids += t; t}
    f match {
      case s: Split => s mapStmt addSKid
      case x: Marker => x mapType addTKid mapExpr addEKid mapStmt addSKid mapPort addPKid
      case c: Circuit => c.modules map addMKid
      case m: DefModule => m map addPKid map addSKid
      case p: DefPort => p mapType addTKid
      case s: Statement => s map addTKid map addEKid map addSKid
      case e: Expression => e map addTKid map addEKid
      case t: Type => t map addTKid
    }
    kids
  }

  def setKids(f: FirrtlNode, kids: Seq[FirrtlNode]): FirrtlNode = {
    var kidIndex = 0
    def get[T<: FirrtlNode](): T = {
      val kid = kids(kidIndex)
      kidIndex += 1
      kid.asInstanceOf[T]
    }
    def setMKid(m: DefModule): DefModule = get[DefModule]()
    def setPKid(p: DefPort): DefPort = get[DefPort]()
    def setSKid(s: Statement): Statement = get[Statement]()
    def setEKid(e: Expression): Expression = get[Expression]()
    def setTKid(t: Type): Type = get[Type]()
    f match {
      case s: Split => s mapStmt setSKid
      case x: Marker => x mapType setTKid mapExpr setEKid mapStmt setSKid mapPort setPKid
      case c: Circuit => c map setMKid
      case m: DefModule => m map setPKid map setSKid
      case p: Port => p mapType setTKid
      case s: Statement => s map setTKid map setEKid map setSKid
      case e: Expression => e map setTKid map setEKid
      case t: Type => t map setTKid
    }
  }

  type Labels = mutable.ArrayBuffer[FirrtlNode]

  //implicit def toForest(trees: Seq[Tree]): Forest = Forest(trees, trees.fir)
  implicit def toSeqTree(forest: Forest): Seq[Tree] = forest.trees

  def toForest(f: FirrtlNode): (Forest, Labels) = {
    val labels = new Labels()
    def recToTree(f: FirrtlNode): Tree = {
      val kids = getKids(f)
      val childrenTrees = if(kids.isEmpty) {
        Nil
      } else {
        kids map recToTree
      }
      labels += f
      Tree(labels.size - 1, childrenTrees)
    }
    (Forest(Seq(recToTree(f))), labels)
  }

  def reversePrePostOrder(pre: FirrtlNode => FirrtlNode, post: FirrtlNode => FirrtlNode)(f: FirrtlNode): FirrtlNode = {
    val preF = pre(f)
    val kids = getKids(preF).reverse
    val newKids = kids.map(reversePrePostOrder(pre, post))
    val allF = setKids(preF, newKids.reverse)
    val postF = post(allF)
    postF
  }
  def reversePostOrder(visit: FirrtlNode => FirrtlNode)(f: FirrtlNode): FirrtlNode = {
    val newF = visit(f)
    val kids = getKids(newF).reverse
    val newKids = kids.map(reversePostOrder(visit))
    setKids(newF, newKids.reverse)
  }
  def postOrder(visit: FirrtlNode => FirrtlNode)(f: FirrtlNode): FirrtlNode = {
    val kids = getKids(f)
    val newKids = kids.map(postOrder(visit))
    setKids(visit(f), newKids)
  }
  def preOrder(visit: FirrtlNode => FirrtlNode)(f: FirrtlNode): FirrtlNode = {
    val newF = visit(f)
    val kids = getKids(newF)
    val newKids = kids.map(preOrder(visit))
    setKids(newF, newKids)
  }
  def stringifyAST(firrtlAST: Circuit): String = {
    var ntabs = 0
    val buf = new StringBuilder
    val string = firrtlAST.toString
    string.zipWithIndex.foreach { case (c, idx) =>
      c match {
        case ' ' =>
        case '(' =>
          ntabs += 1
          buf ++= "(\n" + "| " * ntabs
        case ')' =>
          ntabs -= 1
          buf ++= "\n" + "| " * ntabs + ")"
        case ','=> buf ++= ",\n" + "| " * ntabs
        case  c if idx > 0 && string(idx-1)==')' =>
          buf ++= "\n" + "| " * ntabs + c
        case c => buf += c
      }
    }
    buf.toString
  }
}

class Difference(before: CircuitState, after: CircuitState) {

  /**
    * Will walk the buildDiffAST return to add the prefix/postfix around
    * inserted/deleted nodes so calling circuit.serialize is pretty
    *
    * Should check out how to make it colored! Look at dramatic error?
    * @return
    */
  def buildSerializableDiffAST(c: Circuit): Circuit = {
    var mode = "m"
    val visited = mutable.ArrayBuffer[FirrtlNode]()
    implicit class FirrtlNodeWithMap(n: FirrtlNode) {
      def map(f: FirrtlNode => FirrtlNode): FirrtlNode = {
        setKids(n, getKids(n).map(f))
      }
    }
    /*
    val startDel = Console.RED + "|-"
    val startInsert = Console.GREEN + "|+"
    val endDel = "-|" + Console.BLACK
    val endInsert = "+|" + Console.BLACK
    */
    val startDel = Console.RED
    val startInsert = Console.GREEN
    val endDel =  Console.BLACK
    val endInsert = Console.BLACK
    def rec(mode: String)(f: FirrtlNode): FirrtlNode = {
      (mode, f) match {
        //case ("m", d@Delete(x: Block)) => Mark(startDel + "\n", d.ir.map(rec("-")), "\n" + endDel)
        case ("m", Delete(x)) => Mark(startDel, x.map(rec("-")), endDel)
        case ("-", Delete(x)) => x.map(rec("-"))
        case ("+", Delete(x)) => Mark(endInsert + startDel, x.map(rec("-")), endDel + startInsert)
        case ("m", Insert(x)) => Mark(startInsert, x.map(rec("+")), endInsert)
        case ("-", Insert(x)) => Mark(endDel + startInsert, x.map(rec("+")), endInsert+startDel)
        case ("+", Insert(x)) => x.map(rec("+"))
        case ("m", x) => x.map(rec("m"))
        //case ("-", x: Statement) => Mark(endDel+"\n", f.map(rec("m")), "\n"+startDel)
        //case ("+", x: Statement) => Mark(endInsert+"\n", f.map(rec("m")), "\n"+startInsert)
        case ("-", x) => Mark(endDel, f.map(rec("m")), startDel)
        case ("+", x) => Mark(endInsert, f.map(rec("m")), startInsert)
      }
    }
    rec("m")(c).asInstanceOf[Circuit]
  }

  /* Done for now, but need to look into:
      - Whether an inserted node's children are correct
      - needs a more complicated example to check this
   */
  def buildDiffAST(c: Circuit, cost: Cost): Circuit = {
    var order = 0
    var mode: String = "m"
    val costs = cost.asInstanceOf[SeqCost].costs
    val visited = mutable.ArrayBuffer[FirrtlNode]()
    def getCost[T<:FirrtlNode](f: T): T = {
      f match {
        case m: Delete =>
          visited += m.ir
          f
        case m: Insert =>
          visited += m.ir
          f
        case m: Split => f
        case revisit if visited.nonEmpty && visited.last == revisit =>
          visited.remove(visited.size - 1)
          revisit
        case other => {
          val cost = costs(costs.size - 1 - order)
          val c = cost match {
            case Matching(_, ir) => ir
            case Deletion(_, ir) =>
              visited += ir
              Delete(ir)
            case Insertion(_, ir) =>
              visited += ir
              Insert(ir)
            case Swap(_, from, to) => Split(Delete(from), Insert(to))
          }
          /*
          c match {
            case Split(a, b) =>
              println(s"$order: ${getString(f)} \t\t\t\t-> Split(${getString(a)}, ${getString(b)})")
            case _ =>
              println(s"$order: ${getString(f)} \t\t\t\t-> ${getString(c)}")
          }
          */
          order += 1
          c.asInstanceOf[T]
        }
      }
    }
    val ret = reversePostOrder(getCost)(c).asInstanceOf[Circuit]
    //println(ret.serialize)
    //println(stringifyAST(ret))
    ret
  }

  /** Walks two expression trees and returns a sequence of tuples of where they differ */
  def diff(f1: FirrtlNode, f2: FirrtlNode): Seq[(FirrtlNode, FirrtlNode)] = {
    if(f1 == f2) {
      Nil
    } else {
      val (f1Kids, f2Kids) = (getKids(f1), getKids(f2))
      if (f1Kids == Nil || f2Kids == Nil || f1Kids.size != f2Kids.size) {
        Seq((f1, f2))
      } else {
        f1Kids.zip(f2Kids).flatMap { case (f1k, f2k) => diff(f1k, f2k) }
      }
    }
  }


  // For serialization
  case class Mark(begin: String, ir: FirrtlNode, end: String) extends Marker {
    def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(begin, p(ir), end)
    def serialize: String = begin + ir.serialize + end
  }
  /*
  case class StartInsert(ir: FirrtlNode) extends Marker {
    def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(p(ir))
    def serialize: String = ir.serialize
  }
  case class EndInsert(ir: FirrtlNode) extends Marker {
    def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(p(ir))
    def serialize: String = ir.serialize
  }
  case class StartDelete(ir: FirrtlNode) extends Marker {
    def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(p(ir))
    def serialize: String = ir.serialize
  }
  case class EndDelete(ir: FirrtlNode) extends Marker {
    def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(p(ir))
    def serialize: String = ir.serialize
  }
  */

  implicit def seq2cost(costs: Seq[Cost]): Cost = if(costs.isEmpty) SeqCost(0, Nil) else SeqCost(costs.map(_.value).sum, costs)
  implicit def cost2seq(cost: Cost): Seq[Cost] = cost match {
    case SeqCost(_, costs) => costs
    case other => Seq(other)
  }

  def diffMyers(): Cost = ???

  def diffSashaAndZhang(): Cost = {
    // Maps subforests to diff
    def recursiveDiff(F: Forest, G: Forest, C: Cache): Cost = {
      (C.cached(F, G), F.trees, G.trees) match {
        case (Some(cost), _, _) => cost
        case (_, Nil, Nil) => C.update(SeqCost(0, Nil), F, G)
        case (_, _, Nil) =>
          val cost = recursiveDiff(F.dropRightRoot(), G, C) ++ C.deleteCost(F.largestOrder.get)
          C.update(cost, F, G)
        case (_, Nil, _) =>
          val cost = recursiveDiff(F, G.dropRightRoot(), C) ++ C.insertCost(G.largestOrder.get)
          C.update(cost, F, G)
        case (_, _, _) =>
          val blabel = C.beforeLabels(F.largestOrder.get)
          val alabel = C.afterLabels(G.largestOrder.get)
          // Possibility 1: remove F's right-most label
          val cost1: Cost = recursiveDiff(F.dropRightRoot(), G, C) ++ C.deleteCost(F.largestOrder.get)
          // Possibility 2: remove G's right-most label
          val cost2: Cost = recursiveDiff(F, G.dropRightRoot(), C) ++ C.insertCost(G.largestOrder.get)
          // Possibility 3: match F's and G's right most labels
          val restOfForestCost: Cost = recursiveDiff(F.dropRightTree(), G.dropRightTree(), C)
          val subTreeCost: Cost = recursiveDiff(Forest(F.last.children), Forest(G.last.children), C)
          val cost3: Cost = restOfForestCost ++ subTreeCost ++ C.matchCost(F.largestOrder.get, G.largestOrder.get)

          val finalCost: Cost = Seq(cost3, cost1, cost2).minBy(_.value)
          C.update(finalCost, F, G)
      }
    }

    def iterativeDiff(f: Forest, g: Forest, C: Cache): Cost = {
      val inCompute = mutable.HashSet[(Forest, Forest)]()
      val toCompute = mutable.ArrayBuffer[(Forest, Forest)]()

      def add(x: (Forest, Forest)): Unit = {
        if (!inCompute.contains(x)) {
          inCompute += x
          toCompute += x
        }
      }

      def pop(): (Forest, Forest) = {
        val x = toCompute.remove(toCompute.size - 1)
        inCompute -= x
        x
      }

      def peek(): (Forest, Forest) = {
        toCompute.last
      }

      def getOrAdd(f: Forest, g: Forest, o: Option[Cost] = Some(SeqCost(0,Nil))): Option[Cost] = {
        C.cached(f, g) match {
          case Some(childCost) => Some(childCost)
          case None =>
            o match {
              case Some(x) => add((f, g))
              case None =>
            }
            None
        }
      }

      add((f, g))
      var counter = 0
      while (toCompute.nonEmpty) {
        if(counter % 100 == 0) {
          println(s"Iteration $counter, stacksize = ${toCompute.size}, setsize = ${inCompute.size}")
        }
        counter += 1
        //C.cached(F, G)
        val x = peek()
        val F = x._1
        val G = x._2
        (F.trees, G.trees) match {
          case (Nil, Nil) =>
            C.update(SeqCost(0, Nil), F, G)
            pop()
          case (_, Nil) =>
            getOrAdd(F.dropRightRoot(), G) match {
              case None =>
              case Some(c) =>
                C.update(c ++ C.deleteCost(F.largestOrder.get), F, G)
                pop()
            }
          case (Nil, _) =>
            getOrAdd(F, G.dropRightRoot()) match {
              case None =>
              case Some(c) =>
                C.update(c ++ C.insertCost(G.largestOrder.get), F, G)
                pop()
            }
          case (_, _) =>
            // Possibility 1: remove F's right-most label
            val rec1Opt = getOrAdd(F.dropRightRoot(), G)
            val rec2Opt = getOrAdd(F, G.dropRightRoot(), rec1Opt)
            val rec3Opt = getOrAdd(F.dropRightTree(), G.dropRightTree(), rec2Opt)
            val rec4Opt = getOrAdd(Forest(F.last.children), Forest(G.last.children), rec3Opt)
            (rec1Opt, rec2Opt, rec3Opt, rec4Opt) match {
              case (Some(rec1), Some(rec2), Some(rec3), Some(rec4)) =>
                val cost1: Cost = rec1 ++ C.deleteCost(F.largestOrder.get)
                // Possibility 2: remove G's right-most label
                val cost2: Cost = rec2 ++ C.insertCost(G.largestOrder.get)
                // Possibility 3: match F's and G's right most labels
                val restOfForestCost: Cost = rec3
                val subTreeCost: Cost = rec4
                val cost3: Cost = restOfForestCost ++ subTreeCost ++ C.matchCost(F.largestOrder.get, G.largestOrder.get)

                val finalCost: Cost = Seq(cost3, cost1, cost2).minBy(_.value)
                C.update(finalCost, F, G)
                pop()
              case _ =>
            }
        }
      }
      C.cached(f, g).get
    }

    val cache = Cache(before, after)
    //recursiveDiff(cache.beforeForest, cache.afterForest, cache)
    iterativeDiff(cache.beforeForest, cache.afterForest, cache)

  }

  def makeIterator(): Iterator[(FirrtlNode, FirrtlNode)] = diff(before.circuit, after.circuit).toIterator

  private val internalIterator: Iterator[(FirrtlNode, FirrtlNode)] = makeIterator()

  def next(): (FirrtlNode, FirrtlNode) = internalIterator.next()
}

case class Cache(before: CircuitState, after: CircuitState) {
  val table = mutable.HashMap[((Option[Int], Option[Int]), (Option[Int], Option[Int])), Cost]()
  val beforeOrder2Cost = mutable.HashMap[Int, Cost]()
  val (beforeForest, beforeLabels) = toForest(before.circuit)
  //println(beforeLabels.map(_.toString().substring(0, 15)).reverse.mkString("\n"))
  val (afterForest, afterLabels) = toForest(after.circuit)
  def cached(F: Forest, G: Forest): Option[Cost] = {
    table.get((F.range, G.range))
  }
  def update(cost: Cost, F: Forest, G: Forest): Cost = {
    table((F.range, G.range)) = cost
    cost
  }
  def deleteCost(order: Int): Cost = Deletion(1, beforeLabels(order))
  def insertCost(order: Int): Cost = Insertion(1, afterLabels(order))
  def matchCost(from: Int, to: Int): Cost = {
    val (bLabel, aLabel) = (beforeLabels(from), afterLabels(to))
    if(vertexEquals(bLabel, aLabel)) {
      Matching(0, bLabel)
    } else {
      Swap(1, bLabel, aLabel)
    }
  }
}

case class Tree(order: Int, children: Seq[Tree]) {
  def removeRoot(): Seq[Tree] = children
}
object Forest {
  def apply(trees: Seq[Tree]): Forest = {
    if(trees.isEmpty) {
      Forest(Nil, None)
    } else {
      var small = trees.head
      while(small.children != Nil) {
        small = small.children.head
      }
      Forest(trees, Some(small.order))
    }
  }
}
case class Forest(trees: Seq[Tree], smallestOrder: Option[Int]) {
  def dropRightRoot(): Forest = {
    val newTrees = trees.dropRight(1) ++ trees.last.removeRoot()
    val newSmallest = if(newTrees != Nil) smallestOrder else None
    Forest(newTrees, newSmallest)
  }
  def dropRightTree(): Forest = {
    val newTrees = trees.dropRight(1)
    val newSmallest = if(newTrees != Nil) smallestOrder else None
    Forest(newTrees, newSmallest)
  }
  def largestOrder: Option[Int] = if(trees.isEmpty) None else Some(trees.last.order)
  def range: (Option[Int], Option[Int]) = (smallestOrder, largestOrder)
}

trait Cost {
  val value: Int
}
case class SeqCost(value: Int, costs: Seq[Cost]) extends Cost {
  override def toString: String = costs.mkString("\n")
}
case class Matching(value: Int, ir: FirrtlNode) extends Cost {
  override def toString: String = "m " + getString(ir)
}
case class Insertion(value: Int, ir: FirrtlNode) extends Cost {
  override def toString: String = "+ " + getString(ir)
}
case class Deletion(value: Int, ir: FirrtlNode) extends Cost {
  override def toString: String = "- " + getString(ir)
}
case class Swap(value: Int, from: FirrtlNode, to: FirrtlNode) extends Cost {
  override def toString: String = {
    val fromStr = getString(from)
    val toStr = getString(to)
    val maxStrLength = Seq(fromStr, toStr).map(_.length).max
    s"""${">" * maxStrLength}
       |- ${fromStr}
       |${"=" * maxStrLength}
       |+ ${toStr}
       |${"<" * maxStrLength}
       """.stripMargin
  }
}

trait Marker extends DefModule with DefPort with Statement with Expression with Type  {
  def apply(p: FirrtlNode => FirrtlNode): Marker
  override val info = NoInfo
  override val name = ""
  override val direction = Output
  override val tpe = UnknownType
  override val ports = Nil
  override def mapStmt(f: Statement => Statement) = apply { case x: Statement => f(x); case other => other }
  override def mapExpr(f: Expression => Expression) = apply { case x: Expression => f(x); case other => other }
  override def mapType(f: Type => Type) = apply { case x: Type => f(x); case other => other }
  override def mapWidth(f: Width => Width) = apply { case x: Width => f(x); case other => other }
  override def mapPort(f: DefPort => DefPort) = apply { case x: DefPort => f(x); case other => other }
  override def mapString(f: String => String) = this
  override def mapInfo(f: Info => Info) = apply { case x: Info => f(x); case other => other }
  def serialize: String
}
// Diff AST
case class Insert(ir: FirrtlNode) extends Marker {
  def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(p(ir))
  def serialize: String = ir.serialize
}
case class Delete(ir: FirrtlNode) extends Marker {
  def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(p(ir))
  def serialize: String = ir.serialize
}
case class Split(ir0: FirrtlNode, ir1: FirrtlNode) extends Marker {
  def apply(p: FirrtlNode => FirrtlNode): Marker = this.copy(p(ir0), p(ir1))
  def serialize: String = ir0.serialize + ir1.serialize
}

class ImperativeDiffer(t1: String, t2: String) {

  /**
    * Returns the xy coordinates following diagonals, with the depth? Probably for logging?
    * @param depth
    * @param x
    * @param y
    * @return
    */
  def diagonalSlip(depth: Int, x: Int, y: Int): (String, Int, Int) = {
    if (x < t1.length && y < t2.length && t1.charAt(x) == t2.charAt(y))
      diagonalSlip(depth + 1, x + 1, y + 1)
    else {
      val edit = if (depth > 0) s"=$depth" else ""
      (edit, x, y)
    }
  }

  def mergeEditPath(path: List[String], e: String): List[String] = {
    if (path.isEmpty)
      List(e)
    else if (path.last.charAt(0) == e.charAt(0))
      path.dropRight(1) ++ List(path.last + e.drop(1))
    else
      path ++ List(e)
  }

  def mergeDiagonalPath(path: List[String], e: String): List[String] = {
    if (e.isEmpty) path else path ++ List(e)
  }

  /**
    *
    * @param depth Current depth of trace
    * @param visited Set of visited x/y coordinates
    * @param frontier Current points on the frontier, with their associated best paths so far
    * @return
    */
  def sneak(depth: Int, visited: Set[(Int, Int)], frontier: Map[(Int, Int), List[String]]): List[String] = {
    println(s"sneak: $depth " + frontier.keys.map(l => s"(${l._1}, ${l._2})").mkString(" "))
    val found = frontier.get((t1.length, t2.length))
    if (found.isDefined)
      found.get
    else {
      var newFrontier = Map[(Int, Int), List[String]]()
      val newVisited = visited ++ frontier.keys
      for ((key, value) <- frontier) {
        var (x1, y1) = key
        if (x1 < t1.length) {
          val e1 = s"-${t1(x1)}"
          val (e2, xt, yt) = diagonalSlip(0, x1 + 1, y1)
          if (!newVisited((xt, yt)))
            newFrontier += ((xt, yt) -> mergeDiagonalPath(mergeEditPath(value, e1), e2))
        }
        if (y1 < t2.length) {
          val e1 = s"+${t2(y1)}"
          val (e2, xt, yt) = diagonalSlip(0, x1, y1 + 1)
          if (!newVisited((xt, yt)))
            newFrontier += ((xt, yt) -> mergeDiagonalPath(mergeEditPath(value, e1), e2))
        }
      }
      sneak(depth + 1, newVisited, newFrontier)
    }
  }

  def diff(): List[String] = {
    val (path, x, y) = diagonalSlip(0, 0, 0)
    sneak(0, Set[(Int, Int)](), Map[(Int, Int), List[String]]((x, y) -> (if (path.nonEmpty) List(path) else Nil)))
  }

}