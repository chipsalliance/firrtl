package firrtl.passes

import scala.collection.mutable.{ArrayBuffer,HashMap}
import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.Utils._
import firrtl.PrimOps._
import Annotations._
import com.typesafe.scalalogging.LazyLogging

case class ReplaceSeqMemsAnnotation(t: String, tID: TransID)
    extends Annotation with Loose with Unstable {
  val target = CircuitName(t)
  def duplicate(n: Named) = this.copy(t=n.name)
}

object ReplaceSeqMems extends Pass {

  def name = "Replace Sequential Memories with Blackbox + Configuration File"
  
  val smemNames = ArrayBuffer[String]()
  val newModules = ArrayBuffer[DefModule]()

  def replaceSmemRef(e: Expression): Expression = {
    e map replaceSmemRef match {
      case WRef(name,BundleType(fields),MemKind(_),gender) if smemNames.contains(name) => {
        // WRef name == instance name
        val resetField = Field("reset",Default,UIntType(IntWidth(1)))
        WRef(name,BundleType(fields),InstanceKind(),gender)
        //WRef(name,BundleType(fields :+ resetField),InstanceKind(),gender)
      }
      case e => e
    }
  }

  def updateModules(m: Module): Seq[DefModule] = {
    def updateStmts(s: Statement): Statement = s map updateStmts map replaceSmemRef 
    //val updatedStmts = updateStmts(replaceSmem(m.body.stmts))
    val updatedStmts = updateStmts(replaceSmem(m.body))
    val updatedModule = m.copy(body = updatedStmts)
    //println(updatedModule.body.serialize)
    //newModules.toSeq :+ updatedModule
    Seq(updatedModule)
  }

  def run(c: Circuit) = {
    lazy val moduleNamespace = Namespace(c)

    val updatedModulesTemp = c.modules flatMap {
      case m: Module => updateModules(m)
      case m: ExtModule => Seq(m)
    } map {
      case m: Module => m.copy(body = Utils.squashEmpty(m.body))
      case m: ExtModule => m
    }
    val updatedModules = updatedModulesTemp ++ newModules

    smemNames foreach { n =>
      moduleNamespace.newName(n)
      moduleNamespace.newName(n + "_ext")
    }  

    val confFront = Driver.outputPath.split("\\.").init.mkString(".")

    val conf = new java.io.BufferedWriter(new java.io.FileWriter(confFront+".conf"))
    //println("UNIQUE MEMORY MACROS NEEDED")
    uniqueMems foreach { x => 
      printf(x.serialize)
      conf write x.serialize
    }  
    conf.close()

    val out = Circuit(c.info, updatedModules, c.main)
    //println(out.serialize)
    //println(out)
    out
  }

  case class MemProto(
    name: String,
    dataType: String,
    depth: Int,
    writeLatency: Int,
    readLatency: Int,
    numReaders: Int,
    numWriters: Int,
    numRWriters: Int,
    noMask: Boolean,
    mask_gran: Int,
    total_width: Int,
    m: DefMemory
    // TODO: read under write
  ){
    def serialize() = {
      val writers = List.fill(numWriters)(if (noMask) "write" else "mwrite")
      val readers = List.fill(numReaders)("read")
      val readwriters = List.fill(numRWriters)(if (noMask) "rw" else "mrw")
      val ports = (writers ++ readers ++ readwriters).mkString(",")
      "name " + name + "\t" + 
      "depth " + depth + 
      " width " + total_width + 
      " ports " + ports + 
      ( if (!noMask) " mask_gran " + mask_gran else "" ) + 
      "\n"
      //name DataArray_DCache_Data_Array  depth 256 width 128 ports mwrite,read mask_gran 64
      //name ICache_ICache_Data_Array_1  depth 128 width 128 ports rw
      //name ICache_ICache_Tag_Array_1  depth 64 width 42 ports mrw mask_gran 21
    }
  }
  def create_proto(m: DefMemory, noMask: Boolean) = {
    // TODO: Add mask to spec?
    uniqueMems += MemProto(
      m.name + "_ext",
      m.dataType.serialize,
      m.depth,
      m.writeLatency,
      m.readLatency,
      m.readers.length,
      m.writers.length,
      m.readwriters.length,
      noMask,
      baseWidth(m.dataType).intValue,
      totalWidth(m.dataType).intValue,
      m
    )
  }

  val uniqueMems = ArrayBuffer[MemProto]()

  def replaceSmem(s: Statement): Statement = {
    s map replaceSmem match {
      case m: DefMemory if m.readLatency > 0 => {
        val memType = get_type(m).asInstanceOf[BundleType]
        val newMemMod = createSmemMod(m,memType)
        newModules += newMemMod
        smemNames += m.name
        val instName = m.name
        //println(memType)
        //val resetTop = WRef("reset",UIntType(IntWidth(1)),Default,UNKNOWNGENDER)
        //val memRef = WRef(instName,module_type(newMemMod),InstanceKind(),MALE)
        //val resetInt = WSubField(memRef,"reset",UIntType(IntWidth(1)),UNKNOWNGENDER)
        WDefInstance(m.info, instName, m.name, module_type(newMemMod)) 
        //Block(WDefInstance(m.info, instName, m.name, module_type(newMemMod)) :+ Connect(NoInfo,resetInt,resetTop))
      }
      case s => s
    }
  }

  // Derived from create_exps
  def flattenMemVec(dataBlockWidth:BigInt, e: Expression): Seq[Expression] = {
    Utils.tpe(e) match {
      case t:BundleType => {
        t.fields.flatMap { f => 
          flattenMemVec(dataBlockWidth,WSubField(e,f.name,f.tpe,times(gender(e), f.flip))) 
        }
      }
      case t:VectorType => {
        // All vecs concatenated (TODO: should possibly be conditionally in the future?)
        // Check vec sizes equal
        val vecSize = t.size
        val newType = t.tpe match {
          case x: SIntType => SIntType(IntWidth(vecSize * dataBlockWidth)) 
          case x: UIntType => UIntType(IntWidth(vecSize * dataBlockWidth))
        }
        val name = e.asInstanceOf[WSubField].name
        val ref = e.asInstanceOf[WSubField].exp
        if (name == "data" || name == "rdata" || name == "wdata") // || name == "mask")
          Seq(WSubField(ref,name,newType,gender(e)))
        else if (name == "mask")
          Seq(WSubField(ref,name,UIntType(IntWidth(vecSize)),gender(e)))
        else Seq(e)
      }
      case t: UIntType => {
        // TODO: Is this comprehensive? If mask isn't Vec, then don't need mask?
        val ref = e.asInstanceOf[WSubField].exp
        val name = e.asInstanceOf[WSubField].name
        if (name == "mask") Seq(WSubField(ref,"NOTUSED",t,gender(e)))
        else Seq(e)
      }
      case t => Seq(e) 
    }
  }

  // Other types not supported? TODO
  // All R,W,RWers should have the same type?
  // Check that mask Vec length matches data Vec length TODO
  def baseWidth(dataType: Type) : BigInt = {
    dataType match {
      case t: SIntType => t.width.asInstanceOf[IntWidth].width
      case t: UIntType => t.width.asInstanceOf[IntWidth].width
      case t: VectorType => baseWidth(t.tpe)
      case t => -1
    }
  }

  // TODO: Remove redundancy
  def totalWidth(dataType: Type): BigInt = {
    dataType match {
      case t: SIntType => t.width.asInstanceOf[IntWidth].width
      case t: UIntType => t.width.asInstanceOf[IntWidth].width
      case t: VectorType => baseWidth(t.tpe)*t.size
      case t => -1
    }
  }

  def baseTypeWithNewWidth(t: Type, width: BigInt): Type = {
    t match {
    case x: SIntType => SIntType(IntWidth(width)) 
    case x: UIntType => UIntType(IntWidth(width))
    }
  }

  def catVec(t: VectorType, inName: String, inGender: Gender, outName: String) : Seq[Statement] = {
    // Maybe I don't need to cat? TODO
    val delim = "_"
    val newStmts = ArrayBuffer[Statement]()
    val blockWidth = baseWidth(t.tpe)
    (0 until t.size-1) foreach { i =>
      // TODO: Add to namespace?
      val tempNodeName = outName + "_merge" + i
      val high = WRef(inName + delim + (i+1),t.tpe,PortKind(),inGender)
      val low = {
        if (i == 0) WRef(inName + delim + (i),t.tpe,PortKind(),inGender)
        else {
          val prevNodeName = newStmts.last.asInstanceOf[DefNode].name
          val prevNodeType = baseTypeWithNewWidth(t.tpe,blockWidth*(i+1))
          WRef(prevNodeName,prevNodeType,NodeKind(),inGender)
        }
      }
      val newType = baseTypeWithNewWidth(t.tpe,blockWidth*(i+2))
      newStmts += DefNode(NoInfo,tempNodeName,DoPrim(Cat,ArrayBuffer(high,low),ArrayBuffer(),newType))
    }
    newStmts.toSeq
  }

  def map_connect (intPorts:Expression, extRef: WRef, nameMap: Map[String,String], blockWidth: Int) : Seq[Statement] = {

    val delim = "_"
    val intGender = gender(intPorts)

    // Map from Int R/W/RW port name to Ext name
    def lowerExtName(e: Expression): String = e match {
      case e: WRef => if (e.name != "reset") nameMap(e.name) else e.name
      case e: WSubField => lowerExtName(e.exp) + delim + e.name
      case e: WSubIndex => lowerExtName(e.exp) + delim + e.value
    }

    val memModStmts = ArrayBuffer[Statement]()

    tpe(intPorts) match {
      case t: BundleType => {
        t.fields.flatMap {f => 
          val bundleStmt = map_connect(WSubField(intPorts,f.name,f.tpe,times(gender(intPorts),f.flip)),extRef,nameMap, blockWidth)
          memModStmts ++= bundleStmt
          bundleStmt
        }
      }
      case t: VectorType => {

        // TODO: Should only be UInt/SInt?
        val loweredIntName = LowerTypes.loweredName(intPorts)
        val loweredExtName = lowerExtName(intPorts)
        val name = intPorts.asInstanceOf[WSubField].name
        val flattenedWidth = t.size * blockWidth
        val finalType = baseTypeWithNewWidth(t.tpe,flattenedWidth)
  
        //if (name == "mask") {

          // TODO: Mask should always be input?
          /*val maskExpandName = loweredExtName + "_expand"
          val maskExpandType = VectorType(UIntType(IntWidth(1)),flattenedWidth)
          // TODO: Add to namespace
          val maskExpand = DefWire(NoInfo,maskExpandName,maskExpandType)
          memModStmts += maskExpand
          val maskExpr = WRef(maskExpandName,maskExpandType,WireKind(),swap(intGender)) 
          (0 until flattenedWidth) foreach { i =>
            val extLoc = WSubIndex(maskExpr,i,maskExpandType,swap(intGender))
            val intLoc = WSubIndex(intPorts,i / blockWidth,UIntType(IntWidth(1)),intGender)
            val conn = Connect(NoInfo,extLoc,intLoc)
            println(conn.serialize)
            memModStmts += conn
          }*/

          //memModStmts ++= catVec(maskExpandType, maskExpandName, swap(intGender),loweredExtName)

          /*memModStmts ++= catVec(t, loweredExtName + "_flatten", swap(intGender),loweredExtName)

          val extLoc = WSubField(extRef,loweredExtName,finalType,intGender)
          val intLoc = WRef(memModStmts.last.asInstanceOf[DefNode].name,finalType,NodeKind(),swap(intGender))
          memModStmts += Connect(NoInfo,extLoc,intLoc)*/
        //}
        if (name == "data" || name == "rdata" || name == "wdata" || name == "mask"){

          val frontName = loweredExtName.split("_").head
          val backName = loweredExtName.split("_").last
          val portName = {
            if (frontName.startsWith("RW") && backName == "data")  frontName + "_wdata"
            else if (frontName.startsWith("RW") && backName == "mask") frontName + "_wmask"
            else loweredExtName
          }
          // wdata, wmask only associated w/ write port
          val extLoc = WSubField(extRef,portName,finalType,swap(intGender))
          if (intGender == MALE){
            // Write port (blackbox has write data ports concatenated)
            memModStmts ++= catVec(t,loweredIntName,intGender,portName)
            val intLoc = WRef(memModStmts.last.asInstanceOf[DefNode].name,finalType,NodeKind(),intGender)
            memModStmts += Connect(NoInfo,extLoc,intLoc)
          }
          else {
            (0 until t.size) foreach { i =>
              val intLoc = WRef(loweredIntName + delim + (i),t.tpe,PortKind(),intGender)  
              val extract = DoPrim(Bits,ArrayBuffer(extLoc),ArrayBuffer(blockWidth*(i+1)-1,blockWidth*i),t.tpe)
              memModStmts += Connect(NoInfo,intLoc,extract)
            }  
          }
        }
        // TODO: Anything else that can be a Vec? besides data/mask
      }
      case t => {
        val portNameTemp = lowerExtName(intPorts)
        // TODO: Don't use array buffer; use EmptyExpression or similar
        // Don't connect write mask if its a UInt (would be same as WE)
        val ignoreMask = lowerExtName(intPorts).split("_").last == "mask"
        if (!ignoreMask){
          // Mask wouldn't exist here, only data
          //println(portName)
          val frontName = portNameTemp.split("_").head
          val backName = portNameTemp.split("_").last
          val portName = {
            if (frontName.startsWith("RW") && backName == "data")  frontName + "_wdata"
            else portNameTemp
          }
          val extLoc = WSubField(extRef,portName,t,swap(intGender))
          memModStmts += {
            if (intGender == FEMALE) Connect(NoInfo,intPorts,extLoc)
            else Connect(NoInfo,extLoc,intPorts)
          }
        }  
      }

    }
    memModStmts.toSeq
  }  

  def createSmemMod(m: DefMemory, tpe: BundleType): Module = {

    //println(m.serialize)
    //println(m)

    val dataBlockWidth = baseWidth(m.dataType)

    // Unique # for each port
    /*
    val oldMemPortNames = (m.writers ++ m.readers ++ m.readwriters)
    val memPortNames = oldMemPortNames.zipWithIndex.map{case (x,i) => {
      val identifier = {
        if (m.readers.contains(x)) "R"
        else if (m.writers.contains(x)) "W"
        else "RW"
      }
      (x,identifier+i)
    }}.toMap
    */

    val readers = m.readers.zipWithIndex.map{case (r,i) => (r,"R"+i)}
    val writers = m.writers.zipWithIndex.map{case (w,i) => (w,"W"+i)}
    val readwriters = m.readwriters.zipWithIndex.map{case (rw,i) => (rw,"RW"+i)}
    val memPortNames = (readers ++ writers ++ readwriters).toMap

    val memModStmts = ArrayBuffer[Statement]()

    val memPortsTemp = tpe.fields map { f =>
      Port(m.info,f.name,to_dir(toGender(f.flip)),f.tpe)
    } 

    val memPorts = memPortsTemp //:+ Port(m.info,"reset",Input,UIntType(IntWidth(1)))

    //val memPorts = memPortsTemp :+ Port(m.info,"reset",Input,UIntType(IntWidth(1)))

    /*val blackBoxPorts = tpe.fields flatMap { f =>
      val exps = create_exps(WRef(memPortNames(f.name), f.tpe, PortKind(), toGender(f.flip)))
      exps map ( e => Port(m.info, LowerTypes.loweredName(e), to_dir(gender(e)), Utils.tpe(e)) )
    }*/
    
    val blackBoxPortsTemp = tpe.fields flatMap { f =>
      val exps = flattenMemVec(dataBlockWidth,WRef(memPortNames(f.name), f.tpe, PortKind(), toGender(f.flip)))
      exps map ( e => {
        val portNameTemp = LowerTypes.loweredName(e)
        val frontName = portNameTemp.split("_").head
        val backName = portNameTemp.split("_").last
        val portName = {
          if (f.name.split("_").head == "rw" && backName == "data") frontName + "_wdata"
          else if (f.name.split("_").head == "rw" && backName == "mask") frontName + "_wmask"
          else portNameTemp
        }  
        //println(portName)
        //val isMask = portName.split("_").last == "mask"
        Port(m.info, portName, to_dir(gender(e)), Utils.tpe(e)) 
      })
    }

    val blackBoxPorts = blackBoxPortsTemp.toSeq.filter(x => x.name.split("_").last != "NOTUSED") //:+
      //Port(m.info,"reset",Input,UIntType(IntWidth(1)))

    //println(blackBoxPorts)
    val noMask = ((blackBoxPortsTemp.length-1) == blackBoxPorts.length)


    val blackBoxName = m.name + "_ext"
    val memBlackBox = ExtModule(m.info,blackBoxName,blackBoxPorts)
    val blackBoxType = module_type(memBlackBox)
    val blackBoxRef = WRef(blackBoxName,blackBoxType,InstanceKind(),MALE)

   if (uniqueMems.isEmpty){
      // Is this uniquely identifiable
      create_proto(m,noMask)
      newModules += memBlackBox
      memModStmts += WDefInstance(m.info,blackBoxName,blackBoxName,blackBoxType)
    }
    else {
      
      val duplicate = uniqueMems.find(p => {
        (p.dataType == m.dataType.serialize) && 
        (p.depth == m.depth) && 
        (p.writeLatency == m.writeLatency) && 
        (p.readLatency == m.readLatency) && 
        (p.numReaders == m.readers.length) && 
        (p.numWriters == m.writers.length) && 
        (p.numRWriters == m.readwriters.length) 
      })

      if (duplicate == None){
        create_proto(m,noMask)
        newModules += memBlackBox
        memModStmts += WDefInstance(m.info,blackBoxName,blackBoxName,blackBoxType)
      }
      else {
        val oldModName = duplicate.get.name
        memModStmts += WDefInstance(m.info,blackBoxName,oldModName,blackBoxType)
      }
    }  
  
    val connections = memPorts flatMap { p => 
      map_connect(WRef(p.name,p.tpe,PortKind(),to_gender(p.direction)),blackBoxRef,memPortNames,dataBlockWidth.intValue)
    }

    /*
    val connections = memPorts flatMap { p => 
      val intName = p.name
      val outName = memPortNames(p.name)
      val gender = to_gender(p.direction)
      val intExps = create_exps(WRef(intName,p.tpe,PortKind(),gender))
      val outExps = create_exps(WRef(outName,p.tpe,PortKind(),gender))
      intExps.zip(outExps).map{case (i,o) => {
        val gender = i match {
          case w: WSubField => w.gender
          case w: WSubIndex => w.gender
          case w => UNKNOWNGENDER
        }
        val intLoc = WRef(LowerTypes.loweredName(i),i.tpe,PortKind(),gender)
        val outLoc = WSubField(blackBoxRef,LowerTypes.loweredName(o),o.tpe,swap(gender))
        if (gender == FEMALE) Connect(NoInfo,intLoc,outLoc)
        else Connect(NoInfo,outLoc,intLoc)
      }}
    }
    */

    connections foreach {c => memModStmts += c}

    //println(Block(memModStmts.toList).serialize)

    Module(m.info,m.name,memPorts,Block(memModStmts.toList))

  }

}

// TODO: Get rid of extra passes?
// Transform input: Middle Firrtl. Called after "HighFirrtlToMidleFirrtl"
// To use this transform, circuit name should be annotated with its TransId.
class ReplaceSeqMems(transID: TransID) extends Transform with LazyLogging {
  def execute(circuit:Circuit, map: AnnotationMap) = 
    map get transID match {
      case Some(p) => p get CircuitName(circuit.main) match {
        case Some(ReplaceSeqMemsAnnotation(_, _)) => TransformResult((Seq(
          //ReplaceSeqMems
          Legalize,
          ReplaceSeqMems,
          LowerTypes,
          CheckInitialization,
          ResolveKinds,
          InferTypes,
          ResolveGenders
          ) foldLeft circuit){ (c, pass) =>
            val x = Utils.time(pass.name)(pass run c)
            logger debug x.serialize
            x
          }, None, Some(map))
        case _ => TransformResult(circuit, None, Some(map))
      }
      case _ => TransformResult(circuit, None, Some(map))
    }
}

// Have connect to/from maps




















// check that mask is consistent with order of read/write vec
// TODO: Support multiple latencies?
// TODO: Mem tiling pass?

//////////////////////////////////////////////////////////////
// Extra reset