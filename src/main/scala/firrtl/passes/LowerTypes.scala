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

object LowerTypes extends Pass {
  def name = "Lower Types"
  var mname: String = ""
  var sinfo: Info = NoInfo
  
  // Utility functions for lowering names
  private val delim = "_"
  def loweredName(e: Expression): String = e match {
    case e: WRef => e.name
    case e: WSubField => loweredName(e.exp) + delim + e.name
    case e: WSubIndex => loweredName(e.exp) + delim + e.value
  }

  private case class LowerTypesException(msg: String) extends FIRRTLException(msg)
  private def error(msg: String) = 
    throw new LowerTypesException(s"$sinfo: [module $mname] $msg")
  private case class NameMapNode(name: String, elts: Map[String, NameMapNode])

  // Appends $delim to $prefix until no collisions of $prefix + $elts in $names
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

  private def toGender(f: Flip): Gender = f match {
    case DEFAULT => FEMALE
    case REVERSE => MALE
  }
  private def toFlip(g: Gender): Flip = g match {
    case MALE => REVERSE
    case FEMALE => DEFAULT
  }

  // Accepts a Type and an initial namespace
  // Returns new Type with disambiguated names 
  private def disambiguateNames(
      t: BundleType, 
      namespace: HashSet[String]): BundleType = {
    def recDisamNames(t: Type, namespace: HashSet[String]): Type = t match {
      case t: BundleType =>
        // Initialize namespace
        t.fields foreach ( namespace += _.name )
        val newFields = t.fields map { f =>
          if (f.tpe.isAggregate) {
            val tpe = recDisamNames(f.tpe, HashSet())
            val elts = create_exps(WRef("", tpe, PortKind(), toGender(f.flip)))
            val prefix = findValidPrefix(f.name, elts map (loweredName), namespace)
            namespace ++= (elts map (prefix + _))
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
      to: Type): Map[String, NameMapNode] = {
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

  // Creates a Bundle Type from a Stmt
  // Useful for converting body of Module to Type for name disambiguation
  private def stmtToType(s: Stmt): BundleType = {
    def recStmtToType(s: Stmt): Seq[Field] = s match {
      case s: DefWire => Seq(Field(s.name, DEFAULT, s.tpe))
      case s: DefRegister => Seq(Field(s.name, DEFAULT, s.tpe))
      // Use UnknownType because instance ports don't collide in this module 
      case s: WDefInstance => Seq(Field(s.name, DEFAULT, UnknownType()))
      case s: DefMemory => Seq(Field(s.name, DEFAULT, get_type(s)))
      case s: DefNode => Seq(Field(s.name, DEFAULT, UnknownType()))
      case s: Conditionally => recStmtToType(s.conseq) ++ recStmtToType(s.alt)
      case s: Begin => (s.stmts map (recStmtToType)).flatten
      case s => Seq()
    }
    BundleType(recStmtToType(s))
  }

  // Maps names in expression to new disambiguated names
  private def disambiguateExp(
      exp: Expression, 
      map: Map[String, NameMapNode]): Expression = {
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
      nameMap: Map[String, NameMapNode]): Seq[Expression] = {
    val es = create_exps(name, tpe)
    if (nameMap.contains(name)) {
      es map (e => disambiguateExp(e, nameMap.toMap))
    } else {
      es
    }
  }

  // Map module name to port namemap
  private val portNameMap = HashMap[String, Map[String, NameMapNode]]()

  // Each module is its own namespace
  private def lowerTypes(m: Module): Module = {
    val namespace = HashSet[String]() 
    // Map of default loweredNames to loweredNames with disambiguation
    val nameMap = HashMap[String, NameMapNode]()

    def lowerTypesExp(e: Expression): Expression = e match {
      case e: WRef => disambiguateExp(e, nameMap.toMap)
      case e: WSubField => kind(e) match {
        case k: InstanceKind => ???
        case k: MemKind => ???
        case k => 
          val exp = disambiguateExp(e, nameMap.toMap)
          WRef(loweredName(exp), tpe(exp), kind(exp), gender(exp))
      }
      case e: WSubIndex => kind(e) match {
        case k: InstanceKind => ???
        case k: MemKind => ???
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
          if (s.tpe.isGround) { 
            s
          } else {
            val es = createExps(s.name, s.tpe, nameMap.toMap)
            val stmts = es map (e => DefWire(s.info, loweredName(e), tpe(e)))
            Begin(stmts)
          }
        case s: DefRegister =>
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
        case s: WDefInstance => s.tpe match {
          case t: BundleType =>
            val fieldsx = t.fields flatMap { f =>
              val exps = 
                create_exps(WRef(f.name, f.tpe, ExpKind(), times(f.flip, MALE)))
              val disamExps = exps map ( e => 
                disambiguateExp(e, portNameMap(s.module))
              )
              disamExps map { e =>
                // Flip because inst genders are reversed from Module type
                Field(loweredName(e), toFlip(gender(e)).flip, tpe(e))
              }
            }
            WDefInstance(s.info, s.name, s.module, BundleType(fieldsx))
          case _ => error("WDefInstance type should be Bundle!")
        }
        case s: DefMemory => ???
        case s: DefNode => 
          val names = create_exps(s.name, tpe(s.value)) map (lowerTypesExp)
          val exps = create_exps(s.value) map (lowerTypesExp)
          val stmts = names zip exps map { case (n, e) =>
            DefNode(s.info, loweredName(n), e)
          }
          Begin(stmts)
        case s: IsInvalid => s map (lowerTypesExp)
          // TODO handle MemKind?
        case s: Connect => s map (lowerTypesExp)
          // TODO handle MemKind?
        case s => s
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
    // Adds port names to namespace, and local and global namemaps
    def lowerPorts(ports: Seq[Port]): Seq[Port] = {

      val portsType = BundleType(ports map (_.toField))
      val disamPortsType = disambiguateNames(portsType, HashSet())
      val localMap = createNameMapping(portsType, disamPortsType)
      nameMap ++= localMap
      portNameMap += (m.name -> localMap)
      namespace ++= create_exps("", disamPortsType) map 
                    (loweredName) map (_.tail)

      ports zip disamPortsType.fields flatMap { case (p, f) => 
        val es = create_exps(WRef(f.name, f.tpe, PortKind(), toGender(f.flip)))
        es map { e =>
          Port(p.info, loweredName(e), to_dir(gender(e)), tpe(e))
        }
      }
    }

    sinfo = m.info
    m match {
      case m: ExModule => 
        ExModule(m.info, m.name, lowerPorts(m.ports))
      case m: InModule => 
        InModule(m.info, m.name, lowerPorts(m.ports), lowerBody(m.body))
    }
  }

  def run(c: Circuit): Circuit = {
    sinfo = c.info
    Circuit(c.info, c.modules map (lowerTypes), c.main)
  }
}

object OldLowerTypes extends Pass {
   def name = "Lower Types"
   var mname = ""

   // Utility functions for lowering names
   private val delim = "_"
   def loweredName(e: Expression): String = e match {
     case e: WRef => e.name
     case e: WSubField => loweredName(e.exp) + delim + e.name
     case e: WSubIndex => loweredName(e.exp) + delim + e.value
   }
   //// Escape names to prevent collision with lowered names
   //private def escapedName(s: String): String = s.replaceAll("_", "__")
   //private def escapedNameExp(e: Expression): Expression = 
   //  e map (escapedNameExp) map (escapedName)
   //private def escapedNameStmt(s: Stmt): Stmt = 
   //  s map (escapedNameStmt) map (escapedNameExp) map (escapedName)

   def is_ground (t:Type) : Boolean = {
      (t) match {
         case (_:UIntType|_:SIntType) => true
         case (t) => false
      }
   }
   def data (ex:Expression) : Boolean = {
      (kind(ex)) match {
         case (k:MemKind) => (ex) match {
            case (_:WRef|_:WSubIndex) => false
            case (ex:WSubField) => {
               var yes = ex.name match {
                  case "rdata" => true
                  case "data" => true
                  case "mask" => true
                  case _ => false
               }
               yes && ((ex.exp) match {
                  case (e:WSubField) => kind(e).as[MemKind].get.ports.contains(e.name) && (e.exp.typeof[WRef])
                  case (e) => false
               })
            }
            case (ex) => false
         }
         case (k) => false
      }
   }
   def expand_name (e:Expression) : Seq[String] = {
      val names = ArrayBuffer[String]()
      def expand_name_e (e:Expression) : Expression = {
         (e map (expand_name_e)) match {
            case (e:WRef) => names += e.name
            case (e:WSubField) => names += e.name
            case (e:WSubIndex) => names += e.value.toString
         }
         e
      }
      expand_name_e(e)
      names
   }
   def lower_other_mem (e:Expression, dt:Type) : Seq[Expression] = {
      val names = expand_name(e)
      if (names.size < 3) error("Shouldn't be here")
      create_exps(names(0),dt).map{ x => {
         var base = loweredName(x)
         for (i <- 0 until names.size) {
            if (i >= 3) base = base + delim + names(i)
         }
         val m = WRef(base, UnknownType(), kind(e), UNKNOWNGENDER)
         val p = WSubField(m,names(1),UnknownType(),UNKNOWNGENDER)
         WSubField(p,names(2),UnknownType(),UNKNOWNGENDER)
      }}
   }
   def lower_data_mem (e:Expression) : Expression = {
      val names = expand_name(e)
      if (names.size < 3) error("Shouldn't be here")
      else {
         var base = names(0)
         for (i <- 0 until names.size) {
            if (i >= 3) base = base + delim + names(i)
         }
         val m = WRef(base, UnknownType(), kind(e), UNKNOWNGENDER)
         val p = WSubField(m,names(1),UnknownType(),UNKNOWNGENDER)
         WSubField(p,names(2),UnknownType(),UNKNOWNGENDER)
      }
   }
   def merge (a:String,b:String,x:String) : String = a + x + b
   def root_ref (e:Expression) : WRef = {
      (e) match {
         case (e:WRef) => e
         case (e:WSubField) => root_ref(e.exp)
         case (e:WSubIndex) => root_ref(e.exp)
         case (e:WSubAccess) => root_ref(e.exp)
      }
   }
   
   //;------------- Pass ------------------
   
   def lower_types (m:Module) : Module = {
      val mdt = LinkedHashMap[String,Type]()
      mname = m.name
      def lower_types_s (s:Stmt) : Stmt = {
         def lower_mem (e:Expression) : Seq[Expression] = {
            val names = expand_name(e)
            if (Seq("data","mask","rdata").contains(names(2))) Seq(lower_data_mem(e))
            else lower_other_mem(e,mdt(root_ref(e).name))
         }
         def lower_types_e (e:Expression) : Expression = {
            e match {
               case (_:WRef|_:UIntValue|_:SIntValue) => e
               case (_:WSubField|_:WSubIndex) => {
                  (kind(e)) match {
                     case (k:InstanceKind) => {
                        val names = expand_name(e)
                        var n = names(1)
                        for (i <- 0 until names.size) {
                           if (i > 1) n = n + delim + names(i)
                        }
                        WSubField(root_ref(e),n,tpe(e),gender(e))
                     }
                     case (k:MemKind) => {
                        println(s"lower_types_e called on MemKind ${e.serialize} \n $e ")
                        val res = 
                        if (gender(e) != FEMALE) lower_mem(e)(0)
                        else e
                        println(s"  resulting in ${res.serialize} \n $res \n\n")
                        res
                     }
                     case (k) => WRef(loweredName(e),tpe(e),kind(e),gender(e))
                  }
               }
               case (e:DoPrim) => e map (lower_types_e)
               case (e:Mux) => e map (lower_types_e)
               case (e:ValidIf) => e map (lower_types_e)
            }
         }
         s match {
            case (s:DefWire) => {
               if (is_ground(s.tpe)) {
                  s
               } else {
                  val es = create_exps(s.name,s.tpe)
                  val stmts = (es, 0 until es.size).zipped.map{ (e,i) => {
                     DefWire(s.info,loweredName(e),tpe(e))
                  }}
                  Begin(stmts)
               }
            }
            case (s:DefPoison) => {
               if (is_ground(s.tpe)) s else {
                  val es = create_exps(s.name,s.tpe)
                  val stmts = (es, 0 until es.size).zipped.map{ (e,i) => {
                     DefPoison(s.info,loweredName(e),tpe(e))
                  }}
                  Begin(stmts)
               }
            }
            case (s:DefRegister) => {
               if (is_ground(s.tpe)) { 
                  s map (lower_types_e)
               } else {
                  val es = create_exps(s.name,s.tpe)
                  val inits = create_exps(s.init) 
                  val stmts = (es, 0 until es.size).zipped.map{ (e,i) => {
                     val init = lower_types_e(inits(i))
                     DefRegister(s.info,loweredName(e),tpe(e),s.clock,s.reset,init)
                  }}
                  Begin(stmts)
               }
            }
            case (s:WDefInstance) => {
               val fieldsx = s.tpe.as[BundleType].get.fields.flatMap{ f => {
                  val es = create_exps(WRef(f.name,f.tpe,ExpKind(),times(f.flip,MALE)))
                  es.map{ e => {
                     gender(e) match {
                        case MALE => Field(loweredName(e),DEFAULT,f.tpe)
                        case FEMALE => Field(loweredName(e),REVERSE,f.tpe)
                     }
                  }}
               }}
               WDefInstance(s.info,s.name,s.module,BundleType(fieldsx))
            }
            case (s:DefMemory) => {
               mdt(s.name) = s.data_type
               if (is_ground(s.data_type)) s else {
                  val es = create_exps(s.name,s.data_type)
                  val stmts = es.map{ e => {
                     DefMemory(s.info,loweredName(e),tpe(e),s.depth,s.write_latency,s.read_latency,s.readers,s.writers,s.readwriters)
                  }}
                  Begin(stmts)
               }
            }
            case (s:IsInvalid) => {
               val sx = (s map (lower_types_e)).as[IsInvalid].get
               kind(sx.exp) match {
                  case (k:MemKind) => {
                     val es = lower_mem(sx.exp)
                     Begin(es.map(e => {IsInvalid(sx.info,e)}))
                  }
                  case (_) => sx
               }
            }
            case (s:Connect) => {
               val sx = (s map (lower_types_e)).as[Connect].get
               kind(sx.loc) match {
                  case (k:MemKind) => {
                     val es = lower_mem(sx.loc)
                     Begin(es.map(e => {Connect(sx.info,e,sx.exp)}))
                  }
                  case (_) => sx
               }
            }
            case (s:DefNode) => {
               val locs = create_exps(s.name,tpe(s.value))
               val n = locs.size
               val nodes = ArrayBuffer[Stmt]()
               val exps = create_exps(s.value)
               for (i <- 0 until n) {
                  val locx = locs(i)
                  val expx = exps(i)
                  nodes += DefNode(s.info,loweredName(locx),lower_types_e(expx))
               }
               if (n == 1) nodes(0) else Begin(nodes)
            }
            case (s) => s map (lower_types_s) map (lower_types_e)
         }
      }
   
      val portsx = m.ports flatMap { p => 
         val es = create_exps(WRef(p.name,p.tpe,PortKind(),to_gender(p.direction)))
         //es map (escapedNameExp) map { e =>
         es map { e =>
           Port(p.info, loweredName(e), to_dir(gender(e)), tpe(e))
         }
      }
      (m) match {
         case (m:ExModule) => ExModule(m.info,m.name,portsx)
         case (m:InModule) => 
            // In lowering names, need to escape the delimiter
            // TODO Possibly combine with the above for performance
            //val escapedBody = m.body map (escapedNameStmt) map 
            //                    (escapedNameExp) map (escapedName)
            val escapedBody = m.body 
            InModule(m.info,m.name,portsx,lower_types_s(escapedBody))
      }
   }
   
   def run (c:Circuit) : Circuit = {
      val modulesx = c.modules.map(m => lower_types(m))
      Circuit(c.info,modulesx,c.main)
   }
}

