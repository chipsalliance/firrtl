// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>
package firrtl.backends.experimental.mlir.passes

import firrtl.PrimOps.{
  Add,
  And,
  Andr,
  AsAsyncReset,
  AsClock,
  AsFixedPoint,
  AsInterval,
  AsSInt,
  AsUInt,
  Bits,
  Cat,
  Clip,
  Cvt,
  DecP,
  Div,
  Dshl,
  Dshr,
  Eq,
  Geq,
  Gt,
  Head,
  IncP,
  Leq,
  Lt,
  Mul,
  Neg,
  Neq,
  Not,
  Or,
  Orr,
  Pad,
  Rem,
  SetP,
  Shl,
  Shr,
  Squeeze,
  Sub,
  Tail,
  Wrap,
  Xor,
  Xorr
}
import firrtl.Utils.error
import firrtl.annotations.NoTargetAnnotation
import firrtl.backends.experimental.mlir.ops.{
  AnalogType => MAnalogType,
  AsyncResetType => MAsyncResetType,
  BundleType => MBundleType,
  ClockType => MClockType,
  Direction => MDIrection,
  FVectorType => MFVectorType,
  ResetType => MResetType,
  SIntType => MSIntType,
  UIntType => MUIntType,
  _
}
import firrtl.ir.{
  AggregateType,
  AnalogType,
  AsyncResetType,
  Attach,
  Block,
  BundleType,
  Circuit,
  ClockType,
  Conditionally,
  Connect,
  DefInstance,
  DefModule,
  DefNode,
  DefRegister,
  DefWire,
  Default,
  Direction,
  DoPrim,
  DoubleParam,
  Expression,
  ExtModule,
  FileInfo,
  FixedType,
  Flip,
  GroundType,
  Info,
  Input,
  IntParam,
  IntWidth,
  IntervalType,
  IsInvalid,
  Module,
  MultiInfo,
  NoInfo,
  Orientation,
  Output,
  Param,
  PartialConnect,
  Port,
  Print,
  RawStringParam,
  Reference,
  ResetType,
  SIntLiteral,
  SIntType,
  Statement,
  StringParam,
  SubAccess,
  SubField,
  SubIndex,
  UIntLiteral,
  UIntType,
  UnknownType,
  VectorType,
  Verification,
  Width
}
import firrtl.options.CustomFileEmission
import firrtl.options.Viewer.view
import firrtl.stage.FirrtlOptions
import firrtl.{AnnotationSeq, CDefMPort, CDefMemory, CircuitState, DependencyAPIMigration, Namespace, Transform}

case class EmittedMlirCircuitAnnotation(circuitOp: CircuitOp) extends NoTargetAnnotation with CustomFileEmission {
  def baseFileName(annotations: AnnotationSeq): String =
    view[FirrtlOptions](annotations).outputFileName.getOrElse(circuitOp.name)
  def suffix:   Option[String] = Some("mlir")
  def getBytes: Iterable[Byte] = circuitOp.serialize.getBytes
}

object ConvertIR extends Transform with DependencyAPIMigration {
  // To convert FIRRTL from SFC to MFC, I'd like to maintain a minimal dependency.
  // To make it possible to speed up MFC conversion in the future.
  override def invalidates(a: Transform): Boolean = false
  override protected def execute(state: CircuitState): CircuitState = {
    val constantCache = state.circuit.modules.flatMap {
      case Module(_, name, _, _) => Some(name -> collection.mutable.Map[BigInt, String]())
      case _                     => None
    }.toMap
    val ssaNamespaces = state.circuit.modules.flatMap {
      case Module(_, name, _, _) => Some(name -> Namespace())
      case _                     => None
    }.toMap

    def convertCircuit(circuit: Circuit): CircuitOp =
      CircuitOp(circuit.main, circuit.modules.map(convertModule))

    def convertModule(module: DefModule): FIRRTLOp = module match {
      case m: ExtModule =>
        FExtModuleOp(m.name, m.ports.map(convertPort))
      case m: Module =>
        // all type is maintained here, so we don't need InferType pass.
        val typeMap = collection.mutable.Map[Value, Type]()
        // have a constant type cache with (Value, Width) as key.
        val constantCache = collection.mutable.Map[(BigInt, BigInt, Boolean), Value]()
        val ns = Namespace(m)
        // use globalRegion and localRegion to make visitor being able to insert
        val globalRegion = collection.mutable.ArrayBuffer[Op]()
        val localRegion = collection.mutable.ArrayBuffer[Op]()

        // update port type
        m.ports.foreach {
          case Port(_, name, _, tpe) => typeMap.update(name, convertType(tpe))
        }

        def convertExpression(expression: Expression): Expression = expression match {
          case e: Reference =>
            // do nothing if we visit a reference.
            e
          case e: SubField => {
            // recursive convert bundle expression, and finally get the Reference.
            val bundle: Reference = convertExpression(e.expr).asInstanceOf[Reference]
            // query type for field reference(which must be inferred previously).
            val bundleType: MBundleType = typeMap(bundle.name).asInstanceOf[MBundleType]
            // MLIR SubfieldOp use index to locate corresponding field.
            val tpeIdx: Int = bundleType.elements.indexWhere(f => f.name == e.name)
            // create a SSA Value for this SubField and its Type.
            val n: String = ns.newTemp
            // query this type from bundleType
            val subfieldType: FIRRTLType = bundleType.elements(tpeIdx).tpe
            // append subfieldType to typeMap
            typeMap(n) = subfieldType
            // append SubfieldOp to MLIR globalRegion.
            globalRegion += SubfieldOp(
              (n, subfieldType),
              (bundle.name, bundleType),
              tpeIdx
            )
            Reference(n)
          }
          case e: SubIndex => {
            // recursive convert vector expression, and finally get the Reference.
            val vector: Reference = convertExpression(e.expr).asInstanceOf[Reference]
            // query type for field reference(which must be inferred previously).
            val vectorType: MFVectorType = typeMap(vector.name).asInstanceOf[MFVectorType]
            // create a SSA Value for this SubIndex and its Type.
            val n: String = ns.newTemp
            // query this type from vectorType
            val elementType: FIRRTLType = vectorType.elementType
            // append elementType to typeMap
            typeMap(n) = elementType
            // append SubindexOp to MLIR globalRegion.
            globalRegion += SubindexOp(
              (n, elementType),
              (vector.name, vectorType),
              e.value
            )
            Reference(n)
          }
          case e: SubAccess => {
            // recursive convert vector expression, and finally get the Reference.
            val vector: Reference = convertExpression(e.expr).asInstanceOf[Reference]
            // query type for field reference(which must be inferred previously).
            val vectorType: MFVectorType = typeMap(vector.name).asInstanceOf[MFVectorType]

            // recursive convert vector expression, and finally get the Reference.
            val index: Reference = convertExpression(e.index).asInstanceOf[Reference]
            // query type for field reference(which must be inferred previously).
            val indexType: MFVectorType = typeMap(index.name).asInstanceOf[MFVectorType]

            // create a SSA Value for this SubField and its Type.
            val n: String = ns.newTemp
            // query this type from bundleType
            val elementType: FIRRTLType = vectorType.elementType
            // append subfieldType to typeMap
            typeMap(n) = elementType
            // append SubfieldOp to MLIR globalRegion.
            globalRegion += SubaccessOp(
              (n, elementType),
              (vector.name, vectorType),
              (index.name, indexType)
            )
            Reference(n)
          }
          case e: UIntLiteral =>
            Reference(
              // use constantCache to reduce reference counts.
              constantCache.getOrElse(
                (e.value, convertWidth(e.width), true), {
                  val n:            String = ns.newTemp
                  val constantType: FIRRTLType = convertType(e.tpe)
                  typeMap(n) = constantType
                  constantCache.update((e.value, convertWidth(e.width), true), n)
                  globalRegion += ConstantOp((n, constantType), e.value)
                  n
                }
              )
            )

          case e: SIntLiteral =>
            Reference(
              constantCache.getOrElse(
                (e.value, convertWidth(e.width), false), {
                  val n:            String = ns.newTemp
                  val constantType: FIRRTLType = convertType(e.tpe)
                  typeMap(n) = constantType
                  constantCache.update((e.value, convertWidth(e.width), false), n)
                  globalRegion += ConstantOp((n, constantType), e.value)
                  n
                }
              )
            )
          case e: DoPrim =>
            e.op match {
              case _: BinaryPrimOp =>
                val n:          String = ns.newTemp
                val resultType: FIRRTLType = convertType(e.op.propagateType(e))
                typeMap(n) = resultType
                val lhs = convertExpression(e.args(0)).asInstanceOf[Reference]
                val rhs = convertExpression(e.args(1)).asInstanceOf[Reference]
                globalRegion += e.op match {
                  case Add =>
                    AddPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Sub =>
                    SubPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Mul =>
                    MulPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Div =>
                    DivPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Rem =>
                    RemPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case And =>
                    AndPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Or =>
                    OrPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Xor =>
                    XorPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Leq =>
                    LEQPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Lt =>
                    LTPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Geq =>
                    GEQPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Gt =>
                    GTPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Eq =>
                    EQPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Neq =>
                    NEQPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Cat =>
                    CatPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Dshl =>
                    DShlPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                  case Dshr =>
                    DShrPrimOp((n, resultType), (lhs.name, convertType(lhs.tpe)), (rhs.name, convertType(rhs.tpe)))
                }
                Reference(n)
              case _: UnaryPrimOp =>
                val n:          String = ns.newTemp
                val resultType: FIRRTLType = convertType(e.op.propagateType(e))
                typeMap(n) = resultType
                val input = convertExpression(e.args(0)).asInstanceOf[Reference]
                globalRegion += e.op match {
                  case AsSInt       => AsSIntPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case AsUInt       => AsUIntPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case AsAsyncReset => AsAsyncResetPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case AsClock      => AsClockPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case Cvt          => CvtPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case Neg          => NegPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case Not          => NotPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case Andr         => AndRPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case Orr          => OrRPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                  case Xorr         => XorRPrimOp((n, resultType), (input.name, convertType(input.tpe)))
                }
                Reference(n)
              case _: BinaryIntPrimOp =>
                val n:          String = ns.newTemp
                val resultType: FIRRTLType = convertType(e.op.propagateType(e))
                typeMap(n) = resultType
                val input = convertExpression(e.args(0)).asInstanceOf[Reference]
                globalRegion += e.op match {
                  case Head => HeadPrimOp((n, resultType), (input.name, convertType(input.tpe)), e.consts(0))
                  case Tail => TailPrimOp((n, resultType), (input.name, convertType(input.tpe)), e.consts(0))
                  case Shl  => ShlPrimOp((n, resultType), (input.name, convertType(input.tpe)), e.consts(0))
                  case Shr  => ShrPrimOp((n, resultType), (input.name, convertType(input.tpe)), e.consts(0))
                  case Pad  => PadPrimOp((n, resultType), (input.name, convertType(input.tpe)), e.consts(0))
                }
                Reference(n)

              case Bits =>
                val n:          String = ns.newTemp
                val resultType: FIRRTLType = convertType(e.op.propagateType(e))
                typeMap(n) = resultType
                val input = convertExpression(e.args(0)).asInstanceOf[Reference]
                globalRegion += BitsPrimOp(
                  (n, resultType),
                  (input.name, convertType(input.tpe)),
                  e.consts(0),
                  e.consts(1)
                )
                Reference(n)

              case AsInterval | AsFixedPoint | IncP | DecP | SetP | Wrap | Clip | Squeeze =>
                error("not support FixedPoint and Range")
              case _ => error("unknown Op, need remove WIR.")
            }
        }
        def visitStatement(statement: Statement): Unit = statement match {
          case s: Block => s.stmts.map(visitStatement)
          case s: DefNode => convertExpression(s.value)
          case s: DefWire        =>

          case s: DefRegister    =>
          case s: CDefMPort      =>
          case s: CDefMPort      =>
          case s: Connect        =>
          case s: PartialConnect =>
          case s: Attach         =>
          case s: IsInvalid      =>
          case s: DefInstance    =>
          case s: Print          =>
          case s: Verification   =>
          // we support CHIRRTL since MFC already supported.
          case s: CDefMemory =>
          // we support `when` block since MFC
          case s: Conditionally =>

        }
        // start to visit statement
//        visitStatement(m.body)

        FModuleOp(m.name, m.ports.map(convertPort), globalRegion)
    }

    def convertType(tpe: firrtl.ir.Type): FIRRTLType = {
      tpe match {
        case aggregateType: AggregateType =>
          aggregateType match {
            case BundleType(fields) =>
              new MBundleType(fields.map(f => new BundleElement(f.name, convertFlip(f.flip), convertType(f.tpe))))
            case VectorType(tpe, size) => new MFVectorType(convertType(tpe), size)
          }
        case groundType: GroundType =>
          groundType match {
            case AnalogType(width) => new MAnalogType(convertWidth(width))
            case AsyncResetType    => new MAsyncResetType
            case ClockType         => new MClockType
            case ResetType         => new MResetType
            case SIntType(width)   => new MSIntType(convertWidth(width))
            case UIntType(width)   => new MUIntType(convertWidth(width))
            case _: FixedType | _: IntervalType => error("fixed and interval are not supported by mlir.")
          }
        case UnknownType => error("internal error, type should be known after InferType.")
      }
    }
    def convertPort(port: Port): PortInfo =
      PortInfo(port.name, convertType(port.tpe), convertDirection(port.direction))
    def convertWidth(width: Width): BigInt = width match {
      case width: IntWidth => width.width
      case _ => -1
    }
    def convertFlip(orientation: Orientation): Boolean = orientation match {
      case Default => false
      case Flip    => true
    }
    def convertDirection(direction: Direction): MDIrection = direction match {
      case Input  => In
      case Output => Out
    }
    // TODO
    def convertParam(param: Param): String = param match {
      case IntParam(name, value)       => ""
      case DoubleParam(name, value)    => ""
      case StringParam(name, value)    => ""
      case RawStringParam(name, value) => ""
    }
    // TODO
    def convertInfo(info: Info): String = info match {
      case FileInfo(escaped) => ""
      case MultiInfo(infos)  => ""
      case NoInfo            => ""
    }
    state.copy(annotations =
      state.annotations :+ EmittedMlirCircuitAnnotation(
        convertCircuit(state.circuit)
      )
    )
  }
}
