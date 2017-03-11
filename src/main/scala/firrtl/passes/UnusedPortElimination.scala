// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import collection.mutable.{HashSet, HashMap}

import annotation.tailrec

object UnusedPortElimination extends Pass {
  def name = "Unused Port Elimination"

  private def usefulPorts(s: Statement): HashSet[String] = {
    val referenced = HashSet[String]()
    var invalid = List[String]()
    var binding = List[(String, String)]()

    def checkExpressionUse(e: Expression): Expression = {
      e match {
        case WRef(name, _, _, _) => referenced += name
        case _ => e map checkExpressionUse
      }
      e
    }

    def checkUse(s: Statement): Statement = {
      s match {
        case IsInvalid(_, x: WRef) => invalid = x.name :: invalid
        case Connect(_, loc: WRef, expr: WRef) => binding = ((loc.name, expr.name)) :: binding
        case PartialConnect(_, loc: WRef, expr: WRef) => binding = ((loc.name, expr.name)) :: binding
        case _ => Unit
      }
      s map checkUse map checkExpressionUse
    }

    checkUse(s)

    // DFS
    val edges = binding.groupBy(_._2).mapValues(_.map(_._1))
    var seen = HashSet[String]()
    while (!invalid.isEmpty) {
      val head = invalid.head
      invalid = invalid.tail
      if (!seen.contains(head)) {
        seen += head
        invalid = edges.getOrElse(head, Nil) ++ invalid
      }
    }

    referenced -- seen
  }

  private def remove_root(ex: Expression): Expression = ex match {
    case ex: WSubField => ex.exp match {
      case (e: WSubField) => remove_root(e)
      case (_: WRef) => WRef(ex.name, ex.tpe, InstanceKind, UNKNOWNGENDER)
    }
    case _ => error("Shouldn't be here")
  }

  private def cutPorts(killed: HashMap[(String, String), Direction])(s: Statement): Statement = s match {
    case i: WDefInstanceConnector => {
      def obj(p: Expression) = (i.module, LowerTypes.loweredName(remove_root(p)))
      val (kill, keep) = i.portCons.partition { case (p, _) => killed.contains(obj(p)) }
      val invalids = kill.map { case (p, x) =>
        killed(obj(p)) match {
          case Input => EmptyStmt
          case Output => IsInvalid(i.info, x)
      } }
      Block(invalids :+ i.copy(portCons = keep))
    }
    case _ => s map cutPorts(killed)
  }

  @tailrec
  def run(c: Circuit): Circuit = {
    val killed = HashMap[(String, String), Direction]()

    // Remove unused ports
    val modulesx = c.modules.map {
      case m: ExtModule => m
      case m: Module => {
        val useful = usefulPorts(m.body)
        val (keep, kill) = m.ports partition { x => useful.contains(x.name) }
        kill.foreach { case p: Port => killed += ((m.name, p.name) -> p.direction) }
        Module(m.info, m.name, keep, m.body)
      }
    }

    // Remove references to unused ports
    val modulesy = modulesx.map {
      case m: ExtModule => m
      case m: Module => { Module(m.info, m.name, m.ports, cutPorts(killed)(m.body)) }
    }

    val out = Circuit(c.info, modulesy, c.main)
    if (killed.isEmpty) out else run(DeadCodeElimination.run(out))
  }
}
