// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import firrtl.PrimOps._

object FixReferences extends Pass {
  def name = this.getClass.getName
  def run(c: Circuit): Circuit = c.copy(modules = c.modules map onModule)

  // Holds declarations we have seen so far
  type Declarations = collection.mutable.HashSet[String]

  // Holds references we have seen, but have not been declared
  type References = collection.mutable.HashMap[String, WRef]

  // Holds references we have seen in a given statement, but have not been declared
  type RefsInStatements = collection.mutable.HashSet[WRef]

  private def onModule(m: DefModule): DefModule = {
    val declarations = new Declarations
    val references = new References
    val namespace = Namespace(m)
    m.ports.foreach{ p =>
      declarations += p.name
    }
    //println("BEFORE")
    //println(m.serialize)
    //println("AFTER")
    val mx = m map onStmt(declarations, references, namespace)
    //println(mx.serialize)
    mx
  }
  private def onStmt(declarations: Declarations, references: References, namespace: Namespace)(s: Statement): Statement = {
    val refsInStatement = new RefsInStatements
    val updatedS = s map onExp(refsInStatement, declarations, references, namespace)
    val afterDec = s match {
      case sx: HasName if references.contains(sx.name) =>
        declarations += sx.name
        val temp = references(sx.name)
        references -= sx.name
        Seq(Connect(get_info(sx), WRef(temp.name, temp.tpe, WireKind, FEMALE), WRef(sx.name, temp.tpe, temp.kind, MALE)))
      case sx: HasName =>
        declarations += sx.name
        Nil
      case x => Nil
    }
    val beforeDec = (refsInStatement.map { case WRef(n, t, _, _) =>
      DefWire(get_info(s), n, t)
    }).toSeq
    Block(beforeDec ++ Seq(updatedS map onStmt(declarations, references, namespace)) ++ afterDec)
  }
  private def onExp(
      refsInStatement: RefsInStatements,
      declarations: Declarations,
      references: References,
      namespace: Namespace
  )(e: Expression): Expression = e match {
    case WRef(n, t, k, g) => (declarations.contains(n), references.contains(n)) match {
      case (false, false) =>
        val temp = WRef(namespace.newName(n), t, k, g)
        refsInStatement += temp
        references(n) = temp
        temp.copy(kind = WireKind)
      case (false, true) => references(n).copy(kind = WireKind)
      case (true, _) => e
    }
    case e => e map onExp(refsInStatement, declarations, references, namespace)
  }
}
