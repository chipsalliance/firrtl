// See LICENSE for license details.

package firrtl

import scala.collection.mutable
import scala.collection.mutable.HashSet
import firrtl.ir._
import Mappers._

class Namespace private {
  private val tempNamePrefix: String = "_GEN"
  // Begin with a tempNamePrefix in namespace so we always have a number suffix
  private val namespace = mutable.HashSet[String](tempNamePrefix)
  private var n = 0L

  def tryName(value: String): Boolean = {
    val unused = !contains(value)
    if (unused) namespace += value
    unused
  }

  def contains(value: String): Boolean = namespace.contains(value)

  def newName(value: String): Id = {
    var str = value
    while (!tryName(str)) {
      str = s"${value}_$n"
      n += 1
    }
    IdNamespace.allocate(str)
  }
  def newTemp: Id = newName(tempNamePrefix)
}

// TODO the lookups here can't be good
// Perhaps we need a different abstraction
object Namespace {
  // Initializes a namespace from a Module
  def apply(m: DefModule): Namespace = {
    val namespace = new Namespace

    def buildNamespaceStmt(s: Statement): Seq[Id] = s match {
      case s: IsDeclaration => Seq(s.name)
      case s: Conditionally => buildNamespaceStmt(s.conseq) ++ buildNamespaceStmt(s.alt)
      case s: Block => s.stmts flatMap buildNamespaceStmt
      case _ => Nil
    }
    namespace.namespace ++= m.ports.map(p => IdNamespace.lookupName(p.name))
    m match {
      case in: Module =>
        namespace.namespace ++= buildNamespaceStmt(in.body).map(IdNamespace.lookupName(_))
      case _ => // Do nothing
    }

    namespace
  }

  /** Initializes a [[Namespace]] for [[ir.Module]] names in a [[ir.Circuit]] */
  def apply(c: Circuit): Namespace = {
    val namespace = new Namespace
    // TODO this can't be good
    namespace.namespace ++= c.modules.map(m => IdNamespace.lookupName(m.name))
    namespace
  }

  /** Initializes a [[Namespace]] from arbitrary strings **/
  def apply(names: Seq[String] = Nil): Namespace = {
    val namespace = new Namespace
    namespace.namespace ++= names
    namespace
  }
}

