// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>
package firrtl.backends.experimental.mlir
import firrtl.Utils.error
import firrtl.backends.experimental.mlir.ops.{
  AddPrimOp,
  AnalogType,
  AndPrimOp,
  AndRPrimOp,
  AsAsyncResetPrimOp,
  AsClockPrimOp,
  AsSIntPrimOp,
  AsUIntPrimOp,
  AssertOp,
  AssumeOp,
  AsyncResetType,
  AttachOp,
  BitsPrimOp,
  BundleElement,
  BundleType,
  CHIRRTLOp,
  CMemoryType,
  CatPrimOp,
  CircuitOp,
  ClockType,
  CombMemOp,
  ConnectOp,
  ConstantOp,
  CoverOp,
  CvtPrimOp,
  DShlPrimOp,
  DShrPrimOp,
  DivPrimOp,
  EQPrimOp,
  FExtModuleOp,
  FIRRTLExprOp,
  FIRRTLType,
  FIRRTLVerifOp,
  FModuleOp,
  FVectorType,
  GEQPrimOp,
  GTPrimOp,
  HeadPrimOp,
  InstanceOp,
  InvalidValueOp,
  LEQPrimOp,
  LTPrimOp,
  MemDirInfer,
  MemDirRead,
  MemDirReadWrite,
  MemDirWrite,
  MemoryPortAccessOp,
  MemoryPortOp,
  MulPrimOp,
  NEQPrimOp,
  NegPrimOp,
  Node,
  NodeOp,
  NotPrimOp,
  OrPrimOp,
  OrRPrimOp,
  PadPrimOp,
  PartialConnectOp,
  PrimOp,
  PrintFOp,
  RUWNew,
  RUWOld,
  RUWUndefined,
  RegOp,
  RegResetOp,
  RemPrimOp,
  ResetType,
  SIntType,
  SeqMemOp,
  ShlPrimOp,
  ShrPrimOp,
  SkipOp,
  StopOp,
  SubPrimOp,
  SubaccessOp,
  SubfieldOp,
  SubindexOp,
  TailPrimOp,
  UIntType,
  WhenOp,
  WireOp,
  XorPrimOp,
  XorRPrimOp
}
object Serializer {
  val NewLine = "\n"
  val Indent = "  "

  def serializeElement(node: ops.FIRRTLType): String = node match {
    case sint:       SIntType => s"sint" + (if (sint.width != -1) s"<${sint.width}>" else "")
    case uint:       UIntType => s"uint" + (if (uint.width != -1) s"<${uint.width}>" else "")
    case analogType: AnalogType => s"analog" + (if (analogType.width != -1) s"<${analogType.width}>" else "")
    case bundleType: BundleType =>
      s"bundle<${bundleType.elements.map {
        case BundleElement(name, flip, element) => s"$name ${if (flip) "flip " else ""}: ${serializeElement(element)}"
      }.mkString(", ")}>"
    case vectorType: FVectorType    => s"vector<${serializeElement(vectorType.elementType)}, ${vectorType.numElements}>"
    case _:          ClockType      => "clock"
    case _:          AsyncResetType => "asyncreset"
    case _:          ResetType      => "reset"
    case _:          CMemoryType    => error("cmemory is not element type.")

  }
  def serialize(node: ops.FIRRTLType): String = node match {
    case sint:       SIntType => s"!firrtl.sint" + (if (sint.width != -1) s"<${sint.width}>" else "")
    case uint:       UIntType => s"!firrtl.uint" + (if (uint.width != -1) s"<${uint.width}>" else "")
    case analogType: AnalogType => s"!firrtl.analog" + (if (analogType.width != -1) s"<${analogType.width}>" else "")
    case bundleType: BundleType =>
      s"!firrtl.bundle<${bundleType.elements.map {
        case BundleElement(name, flip, element) => s"$name ${if (flip) "flip " else ""}: ${serializeElement(element)}"
      }.mkString(", ")}>"
    case CMemoryType(elementType, numElements) => s"!chirrtl.cmemory<${serialize(elementType)}, $numElements>"
    case vectorType: FVectorType =>
      s"!firrtl.vector<${serializeElement(vectorType.elementType)}, ${vectorType.numElements}>"
    case _: ClockType      => "!firrtl.clock"
    case _: AsyncResetType => "!firrtl.asyncreset"
    case _: ResetType      => "!firrtl.reset"

  }

  def write(node: Node)(implicit b: StringBuilder, indentNum: Int): Unit = node match {
    case op: ops.Op =>
      op match {
        case op: CHIRRTLOp =>
          op match {
            case CombMemOp(result) =>
              writeLine(s"%$result = chirrtl.combmem : ${serialize(result._2)}")
            case MemoryPortAccessOp(port, index, clock) =>
              writeLine(
                s"chirrtl.memoryport.access %${port._1}[%${index}], %${clock._1} : !chirrtl.cmemoryport, ${serialize(
                  index._2
                )} ${serialize(clock._2)}"
              )
            case MemoryPortOp(memory, data, port, direction) =>
              writeLine(s"${data._1}, ${port._1} = chirrtl.memoryport ${direction match {
                case _: MemDirInfer.type     => "Infer"
                case _: MemDirRead.type      => "Read"
                case _: MemDirReadWrite.type => "ReadWrite"
                case _: MemDirWrite.type     => "Write"
              }} %$memory: ${serialize(memory._2)} -> (${serialize(data._2)}, ${serialize(port._2)})")
            case SeqMemOp(result, ruw) =>
              writeLine(s"%$result = chirrtl.seqmem : ${ruw match {
                case RUWNew       => "New"
                case RUWOld       => "Old"
                case RUWUndefined => "Undefined"
              }} ${serialize(result._2)}")
          }
        case op: FIRRTLExprOp =>
          op match {
            // Tested
            case SubfieldOp(result, input, fieldIndex) =>
              writeLine(
                s"%${result._1} = firrtl.subfield %${input._1}($fieldIndex) : (${serialize(input._2)}) -> ${serialize(result._2)}"
              )
            // Tested
            case ConstantOp(result, value) =>
              writeLine(s"%${result._1} = firrtl.constant $value : ${serialize(result._2)}")
            case op: FIRRTLVerifOp =>
              op match {
                case AssertOp(clock, predicate, enable, message) =>
                // TODO
                case AssumeOp(clock, predicate, enable, message) =>
                // TODO
                case CoverOp(clock, predicate, enable, message) =>
                // TODO
              }
            case InvalidValueOp(result) =>
              writeLine(s"%${result._1} = firrtl.invalidvalue : ${serialize(result._2)}")
            // Tested
            case op: PrimOp =>
              op match {
                case AddPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.add %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case SubPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.sub %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case MulPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.mul %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case DivPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.div %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case RemPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.rem %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case AndPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.and %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case OrPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.orp %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case XorPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.xor %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case LEQPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.leq %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case LTPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.ltp %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case GEQPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.geq %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case GTPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.gtp %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case EQPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.eqp %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case NEQPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.neq %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case CatPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.cat %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case DShlPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.dsh %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case DShrPrimOp(result, lhs, rhs) =>
                  writeLine(
                    s"%${result._1} = firrtl.dsh %${lhs._1}, %${rhs._1} : (${serialize(lhs._2)}, ${serialize(rhs._2)}) -> ${serialize(result._2)}"
                  )
                case AsSIntPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.asSint %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case AsUIntPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.asUint %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case AsAsyncResetPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.asAsyncReset %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case AsClockPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.asClock %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case CvtPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.cvt %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case NegPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.neg %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case NotPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.not %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case AndRPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.andr %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case OrRPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.orr %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case XorRPrimOp(result, input) =>
                  writeLine(
                    s"%${result._1} = firrtl.xorr %${input._1} : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case HeadPrimOp(result, input, amount: BigInt) =>
                  writeLine(
                    s"%${result._1} = firrtl.head %${input._1}, $amount : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case TailPrimOp(result, input, amount: BigInt) =>
                  writeLine(
                    s"%${result._1} = firrtl.tail %${input._1}, $amount : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case ShlPrimOp(result, input, amount: BigInt) =>
                  writeLine(
                    s"%${result._1} = firrtl.shl %${input._1}, $amount : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case ShrPrimOp(result, input, amount: BigInt) =>
                  writeLine(
                    s"%${result._1} = firrtl.shr %${input._1}, $amount : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case PadPrimOp(result, input, amount: BigInt) =>
                  writeLine(
                    s"%${result._1} = firrtl.pad %${input._1}, $amount : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
                case BitsPrimOp(result, input, high, low) =>
                  writeLine(
                    s"%${result._1} = firrtl.bits %${input._1} $high to $low : (${serialize(input._2)}) -> ${serialize(result._2)}"
                  )
              }
            case SubaccessOp(result, input, index) =>
            case SubindexOp(result, input, index)  =>
          }
        case op: ops.FIRRTLOp =>
          op match {
            // Tested
            case CircuitOp(name, body) =>
              writeLine(s"""firrtl.circuit "$name" {""")
              body.foreach(write(_)(b, indentNum + 1))
              writeLine("}")
            // Tested
            case FModuleOp(name, ports, body) =>
              writeLine(s"firrtl.module @$name(${ports
                .map(p =>
                  s"${p.direction match {
                    case ops.In  => "in"
                    case ops.Out => "out"
                  }} %${p.name}: ${serialize(p.tpe)}"
                )
                .mkString(s", ")}) {")
              body.foreach(write(_)(b, indentNum + 1))
              writeLine("}")
            // Tested
            case InstanceOp(results, instanceName, moduleName) =>
              writeLine(
                s"${results.map(r => s"%${r._1}").mkString(", ")} = firrtl.instance $instanceName @$moduleName(${(
                  results
                    .map(_._2)
                    .map(p =>
                      s"${p.direction match {
                        case ops.In  => "in"
                        case ops.Out => "out"
                      }} ${p.name}: ${serialize(p.tpe)}"
                    )
                  )
                  .mkString(", ")})"
              )
            // Tested
            case ConnectOp(dest, src) =>
              writeLine(s"firrtl.connect %${dest._1}, %${src._1} : ${serialize(dest._2)}, ${serialize(src._2)}")
            // chisel3 won't emit this, TBD
            case PartialConnectOp(dest, src) =>
              writeLine(s"firrtl.partialconnect ${dest._1}, ${src._1} : ${serialize(dest._2)}, ${serialize(src._2)}")
            // Tested
            case AttachOp(arguments) =>
              writeLine(
                s"firrtl.attach ${arguments.map(a => s"%${a._1}").mkString(", ")} : ${arguments.map(a => serialize(a._2)).mkString(", ")}"
              )
            case FExtModuleOp(name, ports) =>
            // Tested
            case WireOp(result) =>
              writeLine(s"%${result._1} = firrtl.wire : ${serialize(result._2)}")
            // Tested
            case RegOp(result, clock) =>
              writeLine(s"%${result._1} = firrtl.reg %${clock._1} : ${serialize(result._2)}")
            // Tested
            case RegResetOp(result, clock, reset, init) =>
              writeLine(
                s"%${result._1} = firrtl.regreset %${clock._1}, %${reset._1}, %${init._1} : ${serialize(
                  reset._2
                )}, ${serialize(init._2)}, ${serialize(result._2)}"
              )
            case SkipOp() =>
              writeLine("firrtl.skip")
            case StopOp(clock, cond, exitCode, name) =>
              writeLine(s"firrtl.stop ${clock._1}, ${cond._1}, $exitCode : $name")
            case PrintFOp(clock, cond, operands, formatSting) =>
            case NodeOp(result, input) =>
              writeLine(s"%${result._1} = firrtl.node %${input._1} : ${serialize(result._2)}")
            case WhenOp(condition, thenRegion, elseRegion) =>
              writeLine(s"firrtl.when %$condition {")
              thenRegion.foreach(write(_)(b, indentNum + 1))
              write("}")
              if (elseRegion.nonEmpty) {
                write(" else {")
                elseRegion.foreach(write(_)(b, indentNum + 1))
                write("}")
              }
              newLine()
            case _ =>
          }
      }
    case op: ops.Type =>
      op match {
        case lType: FIRRTLType =>
          lType match {
            case analogType: AnalogType     =>
            case resetType:  AsyncResetType =>
            case bundleType: BundleType     =>
            case CMemoryType(elementType, numElements) =>
            case clockType:  ClockType   =>
            case vectorType: FVectorType =>
            case resetType:  ResetType   =>
            case intType:    SIntType    =>
            case intType:    UIntType    =>
            case _ =>
          }
        case _ =>
      }
  }
  def writeLine(str: String)(implicit b: StringBuilder, indent: Int): Unit = {
    b ++= (Indent * indent)
    b ++= str
    b ++= NewLine
  }
  def write(str: String)(implicit b: StringBuilder, indent: Int): Unit = {
    b ++= str
  }
  def newLine()(implicit b: StringBuilder): Unit = {
    b ++= NewLine
  }
}
