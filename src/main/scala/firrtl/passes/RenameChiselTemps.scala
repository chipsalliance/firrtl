package firrtl
package passes

import firrtl.Utils.{gender, kind, tpe, md5}
import firrtl.PrimOps.PrimOpImplicits
import scala.collection.mutable
import ReadableUtils._
import Mappers.{ExpMap, StmtMap, ModuleMap}


/**
 * Renames Chisel-generated names that are ground-typed and not ports or instances.
 *
 * A component's first assigned expression (FAE) is the first expression
 * connected to this component, either via a DefNode or Connect statement.
 *
 * New component names take the following form:
 *   __<base>_<hash>_<tag>[_<id>]
 * where <base> is a named signal in the component's FAE, <hash> is the first 5
 * digits of the MD5 hash of the serialized FAE, <tag> represents the general
 * kind of FAE (register, mux, add, etc.).  Finally, if there are name
 * collisions, we append <id> which guarantees the new component name's uniqueness
 */
object RenameChiselTemps extends Pass {
  def name = "Rename Chisel Temporaries"

  /**
   * Returns true if expression is a WRef, ground type, not port or instance,
   * and has a generated name.
   * TODO(izraelevitz): consider a new naming scheme for aggregate-typed
   * components
   */
  def okToReplace(e: Expression): Boolean = e match {
    case WRef(name, UIntType(_)|SIntType(_), WireKind()|RegKind()|NodeKind()|ExpKind(), _) =>
      isChiselTemp(name)
    case _ => false
  }

  /**
   * Returns a new module with its Chisel temporaries rewritten
   */
  def onModule(m: Module): Module = {

    // Maps old rewritable name to its first assigned expression (FAE)
    val allFAEs = mutable.HashMap[String, Expression]()

    // Maps old rewritable name to the new name's base
    val bases = mutable.HashMap[String, String]()

    // Maps old rewritable name to the new name's hash
    val hashes = mutable.HashMap[String, String]()

    // Maps old rewritable name to the new name's tag
    val tags = mutable.HashMap[String, String]()


    /**
     * Builds allFAEs (all first assigned expressions), hashes, and tags by
     * walking the circuit and updating the corresponding hashmap
     */
    def hashModule(m: Module): Module = {

      /**
       * Adds (name -> FAE) to allFAEs if expression is a WRef, ground type,
       * not port or instance, and has a generated name.  If we have seen
       * this name before, then we ignore all future assignments, causing
       * the new name to be based off of the original name's first
       * assignment.
       *
       * Also updates tags and hashes.
       */
      def maybeAdd(e: Expression, value: Expression): Unit = {
        if (okToReplace(e)) {
          val name = e match { case WRef(n, _, _, _) => n }
          if(!allFAEs.contains(name)) {
            allFAEs(name) = value
            val tag = kind(e) match {
              case k: RegKind => "REG"
              case _ => value match {
                case e: DoPrim => e.op.getString().toUpperCase()
                case e: Mux => "MUX"
                case _ => "CMB" // CMB = combinational
              }
            }
            tags(name) = tag
            hashes(name) = md5(value.serialize).slice(0,5)
          }
        }
      }

      /**
       * Recursive. Walks statements to find connections and nodes that can
       * be renamed, and calls maybeAdd to add them to our hashtables.
       */
      def onStmt(s: Stmt): Stmt = s match {
        case s: Connect =>
          maybeAdd(s.loc, s.exp)
          s
        case s: DefNode =>
          maybeAdd(WRef(s.name, tpe(s.value), NodeKind(), MALE), s.value)
          s
        //TODO(izraelevitz): Add case for IsInvalid?
        case s => s map onStmt
      }
      m map onStmt
    }

    /**
     * The function getBase walks an FAE (first assigned expression) to find
     * the base string to use in a name.
     *
     * A base of an FAE (first assigned expression) is the name of the first
     * WRef in the expression, unless this name is a ChiselTemp. In that case,
     * this ChiselTemp is either renameable or not renameable (e.g.
     * bundle-typed or a port).  If it is renameable, we return ChiselTemp's
     * FAE's base. If it is not renameable, we return "GEN".
     *
     * To prevent an infinite loop in the following case (assuming T_0 is a register):
     *   T_0 <= T_0
     * we update visitedNames when calling getBase on a new FAE. If this case does occur,
     * we return "GEN" as the base.
     */
    val visitedNames = mutable.HashSet[String]()
    def getBase(e: Expression): String = e match {
      case e: WRef => e.name match {
        //case FirrtlTempPattern(base, hash, tag, id) => base
        case ChiselTempPattern(_) => {
          if(visitedNames.contains(e.name)) {
            bases(e.name) = "GEN"
            "GEN"
          } else {
            allFAEs.get(e.name) match {
              case None => "GEN"
              case Some(value) =>
                visitedNames += e.name
                val base = getBase(value)
                bases(e.name) = base
                base
            }
          }
        }
        case _ => e.name
      }
      case e: WSubIndex =>  getBase(e.exp)
      case e: WSubField =>  getBase(e.exp)
      case e: WSubAccess => getBase(e.exp)
      case e: Mux =>        getBase(e.cond)
      case e: ValidIf =>    getBase(e.cond)
      case e: UIntValue =>  e.value.toString(16).toUpperCase()
      case e: SIntValue => "s" + e.value.toString(16).toUpperCase()
      case e: DoPrim =>     getBase(e.args(0))
    }

    /**
     * The function insertNames builds newNames and replaces rewritable names
     * with their new name.
     *
     * We walk all statements and expressions in declaration order. The first
     * time a name is seen that is renameable, we generate the new name and
     * add it to newNames. This allows the uniquification step (calling
     * namespace.newName) to follow the declaration order. Future references
     * to a renameable name obtain the new name from newName.
     */
    val namespace = Namespace(m)
    val newNames = mutable.HashMap[String, String]()
    def insertNames(m: Module): Module = {
      def getNewName(oldName: String): String =
        if (allFAEs.contains(oldName)) {
          if (newNames.contains(oldName)) newNames(oldName)
          else {
            val newName = namespace.newName(
              s"__${bases(oldName)}_${hashes(oldName)}_${tags(oldName)}"
            )
            newNames(oldName) = newName
            newName
          }
        } else oldName
      def onExp(e: Expression): Expression = e match {
        case WRef(n, t, k, g) => WRef(getNewName(n), t, k, g)
        case _ => e map onExp
      }
      def onStmt(s: Stmt): Stmt = ((s map onStmt) map onExp) map getNewName
      m map onStmt
    }


    // Build allFAEs, hashes, tags
    hashModule(m)

    // Build bases
    for((oldName, newExpr) <- allFAEs if (!bases.contains(oldName))) {
      bases(oldName) = getBase(allFAEs(oldName))
    }

    // Build newNames and return new Module with new names
    insertNames(m)
  }
  def run(c: Circuit): Circuit = Circuit(c.info, c.modules.map(onModule _), c.main)
}
