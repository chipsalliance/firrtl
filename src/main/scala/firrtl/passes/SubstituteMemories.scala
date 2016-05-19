package firrtl
package passes

import firrtl.Mappers.{ExpMap, StmtMap}
import firrtl.Utils.{get_type, FieldUtils, PortUtils, toGender}

// Thoughts
//  - Consider tpe being a method on stmt, esp for DefMemory -> BundleType
//  - functions like flip on Field, Flip, Port, and Direction
//  - Redoing IR for memory ports would clean this up a bunch
object SubstituteMemories extends Pass {
  def name = "Substitute Memories"

  private val ONE = UIntValue(BigInt(1), IntWidth(1))

  // TODO Can these be merged into IR?
  private def ref(port: MemPort): Expression = Ref(port.name, port.getType)
  private def ref(str: String): Expression = Ref(str, UnknownType())

  // TODO Can we clean this up? Perhaps writer should be Option
  private class LoweringCompiler extends Compiler {
    def transforms(ignore: java.io.Writer) = Seq(
      new IRToWorkingIR,
      new ResolveAndCheck,
      new HighFirrtlToMiddleFirrtl,
      new MiddleFirrtlToLowFirrtl
    )
  }

  // Creates a register pipeline based on the enable signal subfield of the expr
  private def genPipeline(expr: Expression, en: Field, depth: Int): Stmt = {
    require(depth > 0)
    val stmts = (1 until depth) map { i =>
      val loc = Connect(NoInfo, SubIndex(expr, i), SubIndex(expr, i - 1))
      Conditionally(NoInfo, SubField(SubIndex(expr, i - 1), en), loc, Empty())
    }
    Begin(stmts)
  }

  // Generates a functionally equivalent Module from a DefMemory
  private def toModule(name: String, mem: DefMemory): InModule = {
    require(mem.write_latency > 0)
    // TEMPORARY - For migration from Seq[String] DefMemory port IR style
    // Gather ports
    val readPorts = mem.readers map (r => ReadPort(r, mem.data_type, mem.depth))
    val writePorts = mem.writers map (w => WritePort(w, mem.data_type, mem.depth))
    val readWritePorts = mem.readwriters map (rw => ReadWritePort(rw, mem.data_type, mem.depth))

    // Currently only support groundType memories
    writePorts foreach (p => require(p.mask.tpe == UIntType(IntWidth(1))))
    // Currently doens't support readwrite ports
    require(readWritePorts.isEmpty)

    val namespace = Namespace()

    // Create Storage Array
    // TODO fix clock
    val dataName = namespace.newName("array")
    val dataType = VectorType(mem.data_type, mem.depth)
    require(writePorts.size > 0) // until clock fixed
    val wp1 = writePorts.head
    val dataClock = SubField(ref(wp1), wp1.clk)
    val dataInit = WRef(dataName, dataType, RegKind(), BIGENDER)
    val dataArray = DefRegister(mem.info, dataName, dataType, dataClock, Utils.zero, dataInit)

    // ***** Create read ports *****
    val readPortStmts: Seq[Stmt] = readPorts map { r =>
      val stmts = collection.mutable.ArrayBuffer[Stmt]()
      stmts += IsInvalid(mem.info, ref(r))

      val readDataType = BundleType(Seq(r.data.toField.flip(), r.en.toField))
      val (fromArray, toPort) = if (mem.read_latency > 0) {
        // Build Pipelined registers
        val readPLType = VectorType(readDataType, mem.read_latency)
        val readPLName = namespace.newName(r.name + "Pipe")
        val readPLReg = 
          DefRegister(mem.info, readPLName, readPLType, SubField(ref(r), r.clk), Utils.zero, ref(readPLName))
        stmts += readPLReg
        stmts += IsInvalid(mem.info, Ref(readPLReg))
        stmts += genPipeline(Ref(readPLReg), r.en.toField, mem.read_latency)
        (SubIndex(Ref(readPLReg), 0), SubIndex(Ref(readPLReg), mem.read_latency - 1))
      } else { // If latency == 0, fill in with a wire
        val readWireName = namespace.newName(r.name + "Wire")
        val readWire = DefWire(mem.info, readWireName, readDataType)
        stmts += readWire
        stmts += IsInvalid(mem.info, Ref(readWire))
        (Ref(readWire), Ref(readWire))
      }
      // Guard read data with validif for out-of-bounds addresses
      val rdataExp =
        ValidIf(
          DoPrim(LESS_OP, Seq(SubField(ref(r), r.addr), UIntValue(mem.depth)), Seq()), 
          SubAccess(Ref(dataArray), SubField(ref(r), r.addr))
        )
      val con1 = Connect(mem.info, SubField(fromArray, r.en), SubField(ref(r), r.en))
      val con2 = Connect(mem.info, SubField(fromArray, r.data), rdataExp)
      stmts += Conditionally(mem.info, SubField(ref(r), r.en), Begin(Seq(con1, con2)), Empty())

      val con3 = Connect(mem.info, SubField(ref(r), r.data), SubField(toPort, r.data))
      stmts += con3

      Begin(stmts)
    }

    // ***** Create write ports *****
    val writePortStmts: Seq[Stmt] = writePorts map { w =>
      val stmts = collection.mutable.ArrayBuffer[Stmt]()
      stmts += IsInvalid(mem.info, ref(w))
      val writeDataType =
        BundleType(Seq(w.data.toField, w.addr.toField, w.en.toField, w.mask.toField))

      val (fromPort, toArray) = if (mem.write_latency > 1) {
        val numPLReg = mem.write_latency - 1
        val writePLName = namespace.newName(w.name + "Pipe")
        val writePLReg = 
          DefRegister(
            mem.info, 
            writePLName, 
            VectorType(writeDataType, numPLReg), 
            SubField(ref(w), w.clk), 
            Utils.zero, 
            ref(writePLName))
        stmts += writePLReg

        // Generate pipeline
        for (i <- 1 until numPLReg) {
          stmts += Connect(mem.info, SubIndex(Ref(writePLReg), i), SubIndex(Ref(writePLReg), i - 1))
        }
        (SubIndex(Ref(writePLReg), 0), (SubIndex(Ref(writePLReg), numPLReg - 1)))
      } else {
        val writeWireName = namespace.newName(w.name + "Wire")
        val writeWire = DefWire(mem.info, writeWireName, writeDataType)
        stmts += writeWire
        (Ref(writeWire), Ref(writeWire))
      }

      stmts += BulkConnect(mem.info, fromPort, ref(w))

      val assign =
        Connect(mem.info, SubAccess(Ref(dataArray), SubField(toArray, w.addr)), SubField(toArray, w.data))
      val cond = DoPrim(AND_OP, Seq(SubField(toArray, w.en), SubField(toArray, w.mask)), Seq())
      stmts += Conditionally(mem.info, cond, assign, Empty())

      Begin(stmts)
    }

    // Create ports
    val ports = (readPorts ++ writePorts ++ readWritePorts) map ( p =>
      Port(mem.info, p.name, INPUT, p.getType)
    )

    val stmts = Seq(dataArray) ++ readPortStmts ++ writePortStmts
    InModule(mem.info, name, ports, Begin(stmts))
  }
  def run(c: Circuit): Circuit = {
    lazy val moduleNamespace = Namespace(c)
    def onModule(mod: InModule): Seq[Module] = {
      val newModules = collection.mutable.ArrayBuffer[Module]()

      def onStmt(stmt: Stmt): Stmt = stmt map onStmt match {
        case mem: DefMemory =>
          // Require all clocks the same
          val moduleName = moduleNamespace.newName(mem.name + "Mem")
          val highModule = toModule(moduleName, mem)
          //println(highModule.serialize)
          //val lowModule = lower(highModule)
          //println(lowModule.serialize)
          //newModules += lowModule
          newModules += highModule
          WDefInstance(mem.info, mem.name, moduleName, get_type(mem))
        case s => s
      }
      val modx = mod.copy(body = onStmt(mod.body))
      newModules :+ modx
    }
    val modulesx = c.modules flatMap {
      case m: InModule => onModule(m)
      case m: ExModule => Seq(m)
    } map {
      case m: InModule =>
        m.copy(body = Utils.squashEmpty(m.body))
      case m: ExModule => m
    }
   
    val compiler = new LoweringCompiler
    // TODO don't use null
    compiler.compile(Circuit(c.info, modulesx, c.main), Seq(), null).circuit
  }
}
