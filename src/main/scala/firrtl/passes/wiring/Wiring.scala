package firrtl.passes
package wiring

import firrtl._
import firrtl.ir._
import firrtl.Utils._
import firrtl.Mappers._
import scala.collection.mutable
import firrtl.Annotations._
import WiringUtils._

/** A component, e.g. register etc.
  */
case class SourceAnnotation(target: ComponentName, tID: TransID) extends Annotation with Loose with Unstable {
  def duplicate(n: Named) = n match {
    case n: ComponentName => this copy (target = n)
  }
}

/** A module, e.g. ExtModule etc.
  */
case class SinkAnnotation(target: ModuleName, tID: TransID, pin: String) extends Annotation with Loose with Unstable {
  def duplicate(n: Named) = n match {
    case n: ModuleName => this copy (target = n)
  }
}

case class TopAnnotation(target: ModuleName, tID: TransID) extends Annotation with Loose with Unstable {
  def duplicate(n: Named) = n match {
    case n: ModuleName => this copy (target = n)
  }
}

case class WiringInfo(source: String, comp: String, sinks: Map[String, String], top: String)

class WiringTransform(transID: TransID) extends Transform with SimpleRun {
  def passSeq(wi: WiringInfo) =
    Seq(new Wiring(wi),
        InferTypes,
        ResolveKinds,
        ResolveGenders)
  def execute(c: Circuit, map: AnnotationMap) = map get transID match {
    case Some(p) => 
      val sinks = mutable.HashMap[String, String]()
      val sources = mutable.Set[String]()
      val tops = mutable.Set[String]()
      val comp = mutable.Set[String]()
      p.values.foreach{a =>
        a match {
          case SinkAnnotation(m, _, pin) => sinks(m.name) = pin
          case SourceAnnotation(c, _) =>
            sources += c.module.name
            comp += c.name
          case TopAnnotation(m, _) => tops += m.name
        }
      }
      (sources.size, tops.size, sinks.size, comp.size) match {
        case (0, 0, 0, 0) => TransformResult(c)
        case (1, 1, p, 1) if p > 0 => run(c, passSeq(WiringInfo(sources.head, comp.head, sinks.toMap, tops.head)))
        case _ => error("Wrong number of sources, tops, or sinks!")
      }
    case None => TransformResult(c)
  }
}

case class Lineage(
  name: String,
  children: Seq[(String, Lineage)] = Seq.empty,
  source: Boolean = false,
  sink: Boolean = false,
  sourceParent: Boolean = false,
  sinkParent: Boolean = false,
  sharedParent: Boolean = false,
  addPort: Option[(String, DecType)] = None,
  cons: Seq[(String, String)] = Seq.empty
) {
  def map(f: Lineage => Lineage): Lineage =
    this.copy(children = children.map(c => (c._1, f(c._2))))
  override def toString: String = smallString("")
  def smallString(tab: String): String = s"""
$tab name: $name,
$tab children: ${children.map(c => tab + "   " + c._2.smallString(tab + "    "))}"""
  def serialize(tab: String): String = s"""
$tab name: $name,
$tab source: $source,
$tab sink: $sink,
$tab sourceParent: $sourceParent,
$tab sinkParent: $sinkParent,
$tab sharedParent: $sharedParent,
$tab addPort: $addPort
$tab cons: $cons
$tab children: ${children.map(c => tab + "   " + c._2.serialize(tab + "    "))}"""
}

trait DecType
case object DecInput extends DecType
case object DecOutput extends DecType
case object DecWire extends DecType

class Wiring(wi: WiringInfo) extends Pass {
  def name = this.getClass.getSimpleName
  def toExp(s: String): Expression = {
    def tokenize(s: String): Seq[String] = s.find(c => "[].".contains(c)) match {
      case Some(_) =>
        val i = s.indexWhere(c => "[].".contains(c))
        Seq(s.slice(0, i), s(i).toString) ++ tokenize(s.drop(i + 1))
      case None => Seq(s)
    }
    def parse(tokens: Seq[String]): Expression = {
      val DecPattern = """([1-9]\d*)""".r
      def findClose(tokens: Seq[String], index: Int, nOpen: Int): Seq[String] = tokens(index) match {
        case "[" => findClose(tokens, index + 1, nOpen + 1)
        case "]" if nOpen == 1 => tokens.slice(1, index - 1)
        case _ => findClose(tokens, index + 1, nOpen)
      }
      def buildup(e: Expression, tokens: Seq[String]): Expression = tokens match {
        case "[" :: tail => 
          val indexOrAccess = findClose(tokens, 0, 0)
          indexOrAccess.head match {
            case DecPattern(d) => WSubIndex(e, d.toInt, UnknownType, UNKNOWNGENDER)
            case _ => buildup(WSubAccess(e, parse(indexOrAccess), UnknownType, UNKNOWNGENDER), tokens.tail.slice(0, indexOrAccess.size + 1))
          }
        case "." :: tail =>
          buildup(WSubField(e, tokens(1), UnknownType, UNKNOWNGENDER), tokens.drop(2))
        case Nil => e
      }
      val root = WRef(tokens.head, UnknownType, ExpKind, UNKNOWNGENDER)
      buildup(root, tokens.tail)
    }
    parse(tokenize(s))
  }
  private def onModule(map: Map[String, Lineage], t: Type)(m: DefModule) = {
    map.get(m.name) match {
      case None => m
      case Some(l) =>
        val stmts = mutable.ArrayBuffer[Statement]()
        val ports = mutable.ArrayBuffer[Port]()
        l.addPort match {
          case None =>
          case Some((s, dt)) => dt match {
            case DecInput => ports += Port(NoInfo, s, Input, t)
            case DecOutput => ports += Port(NoInfo, s, Output, t)
            case DecWire =>
              stmts += DefWire(NoInfo, s, t)
          }
        }
        stmts ++= l.cons.map{case ((l, r)) => 
          Connect(NoInfo, toExp(l), toExp(r))
        }
        def onStmt(s: Statement): Statement = Block(Seq(s) ++ stmts)
        m match {
          case Module(i, n, ps, s) => Module(i, n, ps ++ ports, Block(Seq(s) ++ stmts))
          case ExtModule(i, n, ps) => ExtModule(i, n, ps ++ ports)
        }
    }
  }
  private def getType(c: Circuit, module: String, comp: String) = {
    def getRoot(e: Expression): String = e match {
      case r: WRef => r.name
      case i: WSubIndex => getRoot(i.exp)
      case a: WSubAccess => getRoot(a.exp)
      case f: WSubField => getRoot(f.exp)
    }
    val eComp = toExp(comp)
    val root = getRoot(eComp)
    var tpe: Option[Type] = None
    def getType(s: Statement): Statement = s match {
      case DefRegister(_, n, t, _, _, _) if n == root =>
        tpe = Some(t)
        s
      case DefWire(_, n, t) if n == root =>
        tpe = Some(t)
        s
      case WDefInstance(_, n, m, t) if n == root => 
        tpe = Some(t)
        s
      case DefNode(_, n, e) if n == root =>
        tpe = Some(e.tpe)
        s
      case sx: DefMemory =>
        tpe = Some(MemPortUtils.memType(sx))
        sx
      case sx => sx map getType
    }
    val m =  c.modules.filter(_.name == module) match {
      case Seq(mod) => mod
      case _ => error("Must have a module named $module")
    }
    m match {
      case Module(i, n, ps, b) => getType(b)
      case e: ExtModule => error("Module $module cannot be an external module")
    }
    tpe match {
      case None => error("Didn't find register $comp in $module!")
      case Some(t) => 
        def setType(e: Expression): Expression = e map setType match {
          case ex: WRef => ex.copy(tpe = t)
          case ex: WSubField => ex copy (tpe = field_type(ex.exp.tpe, ex.name))
          case ex: WSubIndex => ex copy (tpe = sub_type(ex.exp.tpe))
          case ex: WSubAccess => ex copy (tpe = sub_type(ex.exp.tpe))
        }
        setType(eComp).tpe
    }
  }
  def run(c: Circuit): Circuit = {
    /* check
    there is only one source in shared parent top
    there are no other instance sinks in modules not in scope of wiring pass
    component is a ref (not index, or access, or subfield, or anything)
    */
    val source = wi.source
    val sinks = wi.sinks.keys.toSet
    val compName = wi.comp
    val portNames = c.modules.foldLeft(Map[String, String]()){(map, m) =>
      map + (m.name -> {
        val ns = Namespace(m)
        wi.sinks.get(m.name) match {
          case Some(pin) => ns.newName(pin)
          case None => ns.newName(LowerTypes.loweredName(toExp(compName)))
        }
      })
    }
    val lineages = getLineage(c, wi.top)

    //println(s"""Source Module: $source""")
    //println(s"""Sink Modules: $sinks""")
    //println(s"""Lineages: ${lineages.smallString("")}""")

    val withFields = setSharedParent(wi.top)(setFields(sinks, source)(lineages))
    //println(s"""Lineages with fields: ${withFields.serialize("")}""")

    val withThings = setThings(portNames, compName)(withFields)
    //println(s"""Lineages with things: ${withThings.serialize("")}""")

    val map = pointToLineage(withThings)
    //println(s"""Map: $map""")
    val cx = c.copy(modules = c.modules map onModule(map, getType(c, source, compName)))
    //println(cx.serialize)
    cx
  }
}


object WiringUtils {
  type ChildrenMap = mutable.HashMap[String, Seq[(String, String)]]
  def getChildrenMap(c: Circuit): ChildrenMap = {
    val childrenMap = new ChildrenMap()
    def getChildren(mname: String)(s: Statement): Statement = s match {
      case s: WDefInstance => 
        childrenMap(mname) = childrenMap(mname) :+ (s.name, s.module)
        s
      case s: DefInstance => 
        childrenMap(mname) = childrenMap(mname) :+ (s.name, s.module)
        s
      case s => s map getChildren(mname)
    }
    c.modules.foreach{ m =>
      childrenMap(m.name) = Nil
      m map getChildren(m.name)
    }
    childrenMap
  }

  def getLineage(c: Circuit, top: String): Lineage = {
    val childrenMap = getChildrenMap(c)
    getLineage(childrenMap, top)
  }

  def getLineage(childrenMap: ChildrenMap, module: String): Lineage =
    Lineage(module, childrenMap(module) map (c => (c._1, getLineage(childrenMap, c._2))))

  def setFields(sinks: Set[String], source: String)(lin: Lineage): Lineage = lin map setFields(sinks, source) match {
    case l if sinks.contains(l.name) => l.copy(sink = true)
    case l => 
      val src = l.name == source
      val sinkParent = l.children.foldLeft(false){(b, c) => b || c._2.sink || c._2.sinkParent}
      val sourceParent = if(src) true else l.children.foldLeft(false){(b, c) => b || c._2.source || c._2.sourceParent}
      //println(s"Lineage ${l.name} has sinkParent $sinkParent")
      l.copy(sinkParent=sinkParent, sourceParent=sourceParent, source=src)
  }

  def setSharedParent(top: String)(lin: Lineage): Lineage = lin map setSharedParent(top) match {
    case l if l.name == top => l.copy(sharedParent = true)
    case l => l
  }

  def setThings(portNames:Map[String, String], compName: String)(lin: Lineage): Lineage = {
    val funs = Seq(
      ((l: Lineage) => l map setThings(portNames, compName)),
      ((l: Lineage) => l match {
        case Lineage(name, _, _, _, _, _, true, _, _) => //SharedParent
          l.copy(addPort=Some((portNames(name), DecWire)))
        case Lineage(name, _, _, _, true, _, _, _, _) => //SourceParent
          l.copy(addPort=Some((portNames(name), DecOutput)))
        case Lineage(name, _, _, _, _, true, _, _, _) => //SinkParent
          l.copy(addPort=Some((portNames(name), DecInput)))
        case Lineage(name, _, _, true, _, _, _, _, _) => //Sink
          l.copy(addPort=Some((portNames(name), DecInput)))
        case l => l
      }),
      ((l: Lineage) => l match {
        case Lineage(name, _, true, _, _, _, _, _, _) => //Source
          val tos = Seq(s"${portNames(name)}")
          val from = compName
          l.copy(cons=l.cons ++ tos.map(t => (t, from)))
        case Lineage(name, _, _, _, true, _, _, _, _) => //SourceParent
          val tos = Seq(s"${portNames(name)}")
          val from = l.children.filter{case (i, c) => c.sourceParent}.map{case (i, c) => s"$i.${portNames(c.name)}"}.head
          l.copy(cons=l.cons ++ tos.map(t => (t, from)))
        case l => l
      }),
      ((l: Lineage) => l match {
        case Lineage(name, _, _, _, _, true, _, _, _) => //SinkParent
          val tos = l.children.filter{case (i, c) => (c.sinkParent || c.sink) && !c.sourceParent}.map{case (i, c) => s"$i.${portNames(c.name)}"}
          val from = s"${portNames(name)}"
          l.copy(cons=l.cons ++ tos.map(t => (t, from)))
        case l => l
      })
    )
    funs.foldLeft(lin)((l, fun) => fun(l))
  }

  def pointToLineage(lin: Lineage): Map[String, Lineage] = {
    val map = mutable.HashMap[String, Lineage]()
    def onLineage(l: Lineage): Lineage = {
      map(l.name) = l
      l map onLineage
    }
    onLineage(lin)
    map.toMap
  }

  //type ModuleMap = Map[String, DefModule]
  //def getModuleMap(c: Circuit): ModuleMap =
  //  c.modules.foldLeft(Map[String, DefModule]()){ (map, m) => map + (m.name -> m) }

  //def getSharedParent(lineages: Seq[Lineage]): Option[(String, String)] = {
  //  require(lineages.size > 0)
  //  val maxDepth = lineages.map(_.size).reduce(math.min(_, _))
  //  def findSharedParents(depth: Int): Option[(String, String)] = {
  //    if(depth == maxDepth) {
  //      Some(lineages.head(depth - 1))
  //    } else {
  //      val siblings = lineages.map(l => l(depth))
  //      siblings.forall(_ == siblings.head) match {
  //        case true => findSharedParents(depth + 1)
  //        case false if depth == 0 => None
  //        case false => Some(lineages.head(depth - 1))
  //      }
  //    }
  //  }
  //  findSharedParents(0)
  //}

  //def countInstances(parentMap: ParentMap, top: String)(module: String): Int =
  //  getAllParents(parentMap)(module).filter(_ == top).size

  //def inLineage(lineage: Lineage, parent: String): Boolean = 
  //  lineage.map(_ == parent).foldLeft(false)(_ || _)

  //def inAllLineages(lineages: Seq[Lineage], parent: String): Boolean = 
  //  lineages.foldLeft(true)((prevIn, lineage) => prevIn && inLineage(lineage, parent))


  //def getLineage(parentMap: ParentMap, parent: String)(child: String): Set[String] =
  //  getAllParents(parentMap)(child) - getAllParents(parentMap)(parent) - parent

  //def countInstances(childrenMap: ChildrenMap): Map[String, Int] = {
  //  val countChildren = mutable.HashMap[String, (String, Int)]()
  //  childrenMap.keys.foreach{ parent =>
  //    childrenMap(parent).foreach { child =>
  //      countChildren(parent) = 
  //    }
  //  }

  //  val parents = parentMap(child)
  //  parents.size match {
  //    case 0 => 1
  //    case n => parents map countInstances(parentMap) reduce (_ + _)


  //}
}
