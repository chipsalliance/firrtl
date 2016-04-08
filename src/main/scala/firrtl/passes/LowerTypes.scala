/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl.passes

import com.typesafe.scalalogging.LazyLogging
import scala.annotation.tailrec

import firrtl._
import firrtl.Utils._
import firrtl.Mappers._

// Datastructures
import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

// TODO Fix name collision in Memory Types
object LowerTypes extends Pass {
  def name = "Lower Types"

  // Utility functions for lowering names
  private val delim = "_"
  def loweredName(e: Expression): String = e match {
    case e: WRef => e.name
    case e: WSubField => loweredName(e.exp) + delim + e.name
    case e: WSubIndex => loweredName(e.exp) + delim + e.value
  }

  private case class LowerTypesException(msg: String) extends FIRRTLException(msg)
  private def error(msg: String)(implicit sinfo: Info, mname: String) =
    throw new LowerTypesException(s"$sinfo: [module $mname] $msg")
  //private def error(msg: String) =
  //  throw new LowerTypesException("$sinfo: [module $mname] $msg")
  private case class NameMapNode(name: String, elts: Map[String, NameMapNode])

  // Appends delim to prefix until no collisions of prefix + elts in names
  @tailrec
  private def findValidPrefix(
      prefix: String,
      elts: Seq[String],
      namespace: HashSet[String]): String = {
    elts find (elt => namespace.contains(prefix + elt)) match {
      case Some(_) => findValidPrefix(prefix + "_", elts, namespace)
      case None => prefix
    }
  }

  // Accepts a Type and an initial namespace
  // Returns new Type with disambiguated names
  private def disambiguateNames(
      t: BundleType,
      namespace: HashSet[String])
      (implicit sinfo: Info, mname: String): BundleType = {
    def recDisamNames(t: Type, namespace: HashSet[String]): Type = t match {
      case t: BundleType =>
        // Initialize namespace
        t.fields foreach ( namespace += _.name )
        val newFields = t.fields map { f =>
          if (f.tpe.isAggregate) {
            val tpe = recDisamNames(f.tpe, HashSet())
            val elts = create_exps(WRef("", tpe, PortKind(), toGender(f.flip)))
            val prefix = findValidPrefix(f.name, elts map (loweredName), namespace)
            namespace ++= (elts map (e => prefix + loweredName(e)))
            Field(prefix, f.flip, tpe)
          } else {
            f
          }
        }
        BundleType(newFields)
      case t: VectorType =>
        VectorType(recDisamNames(t.tpe, namespace), t.size)
      case t => t
    }
    recDisamNames(t, namespace) match {
      case t: BundleType => t
      case t => error("Shouldn't be here")
    }
  }

  // Creates a mapping from flattened references to members of $from ->
  //   flattened references to members of $to
  private def createNameMapping(
      from: Type,
      to: Type)
      (implicit sinfo: Info, mname: String): Map[String, NameMapNode] = {
    (from, to) match {
      case (from: BundleType, to: BundleType) =>
        (from.fields zip to.fields flatMap { case (f, t) =>
          val eltsMap = createNameMapping(f.tpe, t.tpe)
          if ((f.name != t.name) || (eltsMap.size > 0)) {
            Map(f.name -> NameMapNode(t.name, eltsMap))
          } else {
            Map[String, NameMapNode]()
          }
        }).toMap
      case (from: VectorType, to: VectorType) =>
        createNameMapping(from.tpe, to.tpe)
      case (from, to) =>
        if (from.getClass == to.getClass) Map()
        else error("Types to map between do not match!")
    }
  }

  // Maps names in expression to new disambiguated names
  private def disambiguateExp(
      exp: Expression,
      map: Map[String, NameMapNode])
      (implicit sinfo: Info, mname: String): Expression = {
    // Recursive Helper
    def rec(exp: Expression, map: Map[String, NameMapNode]):
        (Expression, Map[String, NameMapNode]) = exp match {
      case e: WRef =>
        if (map.contains(e.name)) {
          val node = map(e.name)
          (WRef(node.name, e.tpe, e.kind, e.gender), node.elts)
        }
        else (e, Map())
      case e: WSubField =>
        val (subExp, subMap) = rec(e.exp, map)
        val (retName, retMap) =
          if (subMap.contains(e.name)) {
            val node = subMap(e.name)
            (node.name, node.elts)
          } else {
            (e.name, Map[String, NameMapNode]())
          }
        (WSubField(subExp, retName, e.tpe, e.gender), retMap)
      case e: WSubIndex =>
        val (subExp, subMap) = rec(e.exp, map)
        (WSubIndex(subExp, e.value, e.tpe, e.gender), subMap)
      case _ => error("Error! Unsupported Expression in disambiguateExp")
    }
    rec(exp, map)._1
  }

  // Enhanced version of Utils.create_exps
  // Uses nameMap to rename expressions if necessary
  private def createExps(
      name: String,
      tpe: Type,
      nameMap: Map[String, NameMapNode])
      (implicit sinfo: Info, mname: String): Seq[Expression] = {
    val exps = create_exps(name, tpe)
    if (nameMap.contains(name)) {
      exps map (e => disambiguateExp(e, nameMap.toMap))
    } else {
      exps
    }
  }

  private case object EmptyExpression extends Expression
  // Splits an Expression into root Ref and tail
  // This function only supports WRef, WSubField, and WSubIndex
  private def splitRef(
      e: Expression)
      (implicit sinfo: Info, mname: String): (WRef, Expression) = e match {
    case e: WRef => (e, EmptyExpression)
    case e: WSubIndex =>
      val (root, tail) = splitRef(e.exp)
      (root, WSubIndex(tail, e.value, e.tpe, e.gender))
    case e: WSubField =>
      val (root, tail) = splitRef(e.exp)
      tail match {
        case EmptyExpression => (root, WRef(e.name, e.tpe, root.kind, e.gender))
        case exp => (root, WSubField(tail, e.name, e.tpe, e.gender))
      }
    case _ => error(s"This utility function does not support expression $e")
  }

  // Adds a root reference to some SubField/SubIndex chain
  private def mergeRef(
      root: WRef,
      body: Expression)
      (implicit sinfo: Info, mname: String): Expression = body match {
    case e: WRef =>
      WSubField(root, e.name, e.tpe, e.gender) // TODO any "kind" issues?
    case e: WSubIndex =>
      WSubIndex(mergeRef(root, e.exp), e.value, e.tpe, e.gender)
    case e: WSubField =>
      WSubField(mergeRef(root, e.exp), e.name, e.tpe, e.gender)
    case EmptyExpression => root
    case e => error(s"This utility function does not support expression $e")
  }
  // TODO Fix? Probably not the best way to do this
  private def splitMemRef(e1: Expression)(implicit sinfo: Info, mname: String):
      (WRef, WRef, WRef, Option[Expression]) = {
    val (mem, tail1) = splitRef(e1)
    val (port, tail2) = splitRef(tail1)
    tail2 match {
      case e2: WRef =>
        (mem, port, e2, None)
      case _ =>
        val (field, tail3) = splitRef(tail2)
        (mem, port, field, Some(tail3))
    }
  }

  // Everything wrapped in run so that it's thread safe
  def run(c: Circuit): Circuit = {
    // TODO Do we really need both?
    // Debug state
    implicit var mname: String = ""
    implicit var sinfo: Info = NoInfo
    // Global state
    val portNameMap = HashMap[String, Map[String, NameMapNode]]()
    val portTypeMap = HashMap[String, Type]()

    // Creates a Bundle Type from a Stmt
    // Useful for converting body of Module to Type for name disambiguation
    // Needs portTypeMap for WDefInstances
    def stmtToType(s: Stmt): BundleType = {
      // Recursive helper
      def recStmtToType(s: Stmt): Seq[Field] = s match {
        case s: DefWire => Seq(Field(s.name, DEFAULT, s.tpe))
        case s: DefRegister => Seq(Field(s.name, DEFAULT, s.tpe))
        // Use UnknownType because instance ports don't collide in this module
        case s: WDefInstance => Seq(Field(s.name, DEFAULT, portTypeMap(s.module)))
        case s: DefMemory => s.data_type match {
          case (_: UIntType | _: SIntType) =>
            Seq(Field(s.name, DEFAULT, get_type(s)))
          case tpe: BundleType =>
            val newFields = tpe.fields map ( f =>
              DefMemory(s.info, f.name, f.tpe, s.depth, s.write_latency,
                s.read_latency, s.readers, s.writers, s.readwriters)
            ) flatMap (recStmtToType)
            Seq(Field(s.name, DEFAULT, BundleType(newFields)))
          case tpe: VectorType =>
            val newFields = (0 until tpe.size) map ( i =>
              s.copy(name = i.toString, data_type = tpe.tpe)
            ) flatMap (recStmtToType)
            Seq(Field(s.name, DEFAULT, BundleType(newFields)))
          case tpe => error(s"Error! Unsupported mem type: $tpe")
        }
        case s: DefNode => Seq(Field(s.name, DEFAULT, UnknownType()))
        case s: Conditionally => recStmtToType(s.conseq) ++ recStmtToType(s.alt)
        case s: Begin => (s.stmts map (recStmtToType)).flatten
        case s => Seq()
      }
      BundleType(recStmtToType(s))
    }

    def lowerTypes(m: Module): Module = {
      val namespace = HashSet[String]()
      // Map of default loweredNames to loweredNames with disambiguation
      val nameMap = HashMap[String, NameMapNode]()
      val memDataTypeMap = HashMap[String, Type]()

      // Map from instance name to module name
      // TODO replace with Analysis
      val instMap = HashMap[String, String]()

      // Lowers an expression of MemKind
      // Since mems with Bundle type must be split into multiple ground type
      //   mem, references to fields addr, en, clk, and rmode must be replicated
      //   for each resulting memory
      // References to data, mask, and rdata have already been split in expand connects
      //   and just need to be converted to refer to the correct new memory
      def lowerTypesMemExp(e: Expression): Seq[Expression] = {
        val (mem, port, field, tail) = splitMemRef(e)
        //val (newMem, newExp) =
        // Fields that need to be replicated for each resulting mem
        if (Seq("addr", "en", "clk", "rmode").contains(field.name)) {
          require(tail.isEmpty) // there can't be a tail for these
          val memType = memDataTypeMap(mem.name)

          if (memType.isGround) {
            val newMem = disambiguateExp(mem, nameMap.toMap).asInstanceOf[WRef]
            Seq(mergeRef(newMem, mergeRef(port, field)))
          } else {
            val exps = createExps(mem.name, memType, nameMap.toMap)
            exps map { e =>
              val newMemName = loweredName(e)
              val newMem = WRef(newMemName, UnknownType(), kind(mem), UNKNOWNGENDER)
              mergeRef(newMem, mergeRef(port, field))
            }
          }
        // Fields that need not be replicated for each
        // TODO handle disambiguation on subfields?
        } else if (Seq("data", "mask", "rdata").contains(field.name)) {
          val newMem = tail match {
            case Some(e) =>
              val newMemExp = disambiguateExp(mergeRef(mem, e), nameMap.toMap)
              val newMemName = loweredName(newMemExp)
              WRef(newMemName, UnknownType(), kind(mem), UNKNOWNGENDER)
            case None =>
              disambiguateExp(mem, nameMap.toMap).asInstanceOf[WRef]
          }
          Seq(mergeRef(newMem, mergeRef(port, field)))
        } else {
          error(s"Error! Unhandled memory field ${field.name}")
        }
      }

      def lowerTypesExp(e: Expression): Expression = e match {
        case e: WRef => disambiguateExp(e, nameMap.toMap)
        //case e: WSubField => kind(e) match {
        case (_: WSubField | _: WSubIndex) => kind(e) match {
          case k: InstanceKind =>
            val (root, tail) = splitRef(e)
            val disamRoot = disambiguateExp(root, nameMap.toMap).asInstanceOf[WRef]
            val instNameMap = portNameMap(instMap(disamRoot.name))
            val name = loweredName(disambiguateExp(tail, instNameMap))
            WSubField(disamRoot, name, tpe(e), gender(e))
          case k: MemKind =>
            val exps = lowerTypesMemExp(e)
            if (exps.length > 1)
              error("Error! lowerTypesExp called on MemKind SubField that needs" +
                    " to be expanded!")
            exps(0)
          case k =>
            val exp = disambiguateExp(e, nameMap.toMap)
            WRef(loweredName(exp), tpe(exp), kind(exp), gender(exp))
        }
        case e: Mux => e map (lowerTypesExp)
        case e: ValidIf => e map (lowerTypesExp)
        case (_: UIntValue | _: SIntValue) => e
        case e: DoPrim => e map (lowerTypesExp)
      }

      def lowerTypesStmt(s: Stmt): Stmt = {
        s map (lowerTypesStmt) match {
          case s: DefWire =>
            sinfo = s.info
            if (s.tpe.isGround) {
              s
            } else {
              val es = createExps(s.name, s.tpe, nameMap.toMap)
              val stmts = es map (e => DefWire(s.info, loweredName(e), tpe(e)))
              Begin(stmts)
            }
          case s: DefRegister =>
            sinfo = s.info
            if (s.tpe.isGround) {
              s
            } else {
              val es = createExps(s.name, s.tpe, nameMap.toMap)
              val inits = create_exps(s.init) map (lowerTypesExp)
              if (es.length != inits.length)
                error("Error! DefRegister not correctly initialized! " +
                      "This should be detected elsewhere...")
              val stmts = es zip inits map { case (e, i) =>
                DefRegister(s.info, loweredName(e), tpe(e), s.clock, s.reset, i)
              }
              Begin(stmts)
            }
          // Could instead just save the type of each Module as it gets processed
          case s: WDefInstance =>
            sinfo = s.info
            s.tpe match {
              case t: BundleType =>
                val name = if (nameMap.contains(s.name)) nameMap(s.name).name else s.name
                instMap += (name -> s.module) // TODO replace with analysis
                val fieldsx = t.fields flatMap { f =>
                  val exps = create_exps(WRef(f.name, f.tpe, ExpKind(), times(f.flip, MALE)))
                  val disamExps = exps map (e => disambiguateExp(e, portNameMap(s.module)))
                  disamExps map ( e =>
                    // Flip because inst genders are reversed from Module type
                    Field(loweredName(e), toFlip(gender(e)).flip, tpe(e))
                  )
                }
                WDefInstance(s.info, name, s.module, BundleType(fieldsx))
              case _ => error("WDefInstance type should be Bundle!")
            }
          case s: DefMemory =>
            sinfo = s.info
            memDataTypeMap += (s.name -> s.data_type)
            if (s.data_type.isGround) {
              s
            } else {
              val exps = createExps(s.name, s.data_type, nameMap.toMap)
              val stmts = exps map { e =>
                DefMemory(s.info, loweredName(e), tpe(e), s.depth,
                  s.write_latency, s.read_latency, s.readers, s.writers,
                  s.readwriters)
              }
              Begin(stmts)
            }
          case s: DefNode =>
            sinfo = s.info
            val names = create_exps(s.name, tpe(s.value)) map (lowerTypesExp)
            val exps = create_exps(s.value) map (lowerTypesExp)
            val stmts = names zip exps map { case (n, e) =>
              DefNode(s.info, loweredName(n), e)
            }
            Begin(stmts)
          case s: IsInvalid =>
            sinfo = s.info
            kind(s.exp) match {
              case k: MemKind =>
                val exps = lowerTypesMemExp(s.exp)
                Begin(exps map (exp => IsInvalid(s.info, exp)))
              case _ => s map (lowerTypesExp)
            }
          case s: Connect =>
            sinfo = s.info
            kind(s.loc) match {
              case k: MemKind =>
                val exp = lowerTypesExp(s.exp)
                val locs = lowerTypesMemExp(s.loc)
                Begin(locs map (loc => Connect(s.info, loc, exp)))
              case _ => s map (lowerTypesExp)
            }
          case s => s map (lowerTypesExp)
        }
      }

      def lowerBody(s: Stmt): Stmt = {
        val bodyType = stmtToType(s)
        val disamBodyType = disambiguateNames(bodyType, namespace)
        val localMap = createNameMapping(bodyType, disamBodyType)
        nameMap ++= localMap
        // No reason to add to namespace, nothing else uses it
        lowerTypesStmt(s)
      }

      // Disambiguate ports and expand aggregate types
      sinfo = m.info
      mname = m.name
      m match {
        case m: ExModule => m
        case m: InModule =>
          // Adds port names to namespace and namemap
          nameMap ++= portNameMap(m.name)
          namespace ++= create_exps("", portTypeMap(m.name)) map
                        (loweredName) map (_.tail)
          m.copy(body = lowerBody(m.body) )
      }
    }

    def lowerPorts(m: Module): Module = {
      def lowerPorts(ports: Seq[Port]): Seq[Port] = {
        val portsType = BundleType(ports map (_.toField))
        val disamPortsType = disambiguateNames(portsType, HashSet())
        val localMap = createNameMapping(portsType, disamPortsType)
        portNameMap += (m.name -> localMap)
        portTypeMap += (m.name -> disamPortsType)

        ports zip disamPortsType.fields flatMap { case (p, f) =>
          val exps = create_exps(WRef(f.name, f.tpe, PortKind(), toGender(f.flip)))
          exps map { e =>
            Port(p.info, loweredName(e), to_dir(gender(e)), tpe(e))
          }
        }
      }

      sinfo = m.info
      mname = m.name
      m match {
        case m: ExModule => m.copy(ports = lowerPorts(m.ports))
        case m: InModule => m.copy(ports = lowerPorts(m.ports))
      }
    }

    sinfo = c.info
    Circuit(c.info, c.modules map lowerPorts map lowerTypes, c.main)
  }
}

