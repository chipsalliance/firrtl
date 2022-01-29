// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>
package firrtl.backends.experimental.mlir
import firrtl.backends.experimental.mlir.ops.{
  AnalogType,
  AssertOp,
  AssumeOp,
  AsyncResetType,
  AttachOp,
  BundleType,
  CHIRRTLOp,
  CMemoryType,
  CircuitOp,
  ClockType,
  CombMemOp,
  ConnectOp,
  ConstantOp,
  CoverOp,
  FExtModuleOp,
  FIRRTLExprOp,
  FIRRTLType,
  FIRRTLVerifOp,
  FModuleOp,
  FVectorType,
  InstanceOp,
  InvalidValueOp,
  MemoryPortAccessOp,
  MemoryPortOp,
  Node,
  NodeOp,
  PartialConnectOp,
  PrimOp,
  PrintFOp,
  RegOp,
  RegResetOp,
  ResetType,
  SIntType,
  SeqMemOp,
  SkipOp,
  SpecialConstantOp,
  StopOp,
  SubaccessOp,
  SubfieldOp,
  SubindexOp,
  UIntType,
  WhenOp,
  WireOp
}

object Serializer {
  val NewLine = "\n"
  val Indent = "  "
  def serialize(node: ops.FIRRTLType): String = ???
  def serialize(node: ops.PortInfo):   String = ???

  def write(node: Node)(implicit b: StringBuilder, indentNum: Int): Unit = node match {
    case op: ops.Op =>
      op match {
        case op: CHIRRTLOp =>
          op match {
            case CombMemOp(result)                           =>
            case MemoryPortAccessOp(port, index, clock)      =>
            case MemoryPortOp(memory, data, port, direction) =>
            case SeqMemOp(result, ruw)                       =>
            case _                                           =>
          }
        case op: FIRRTLExprOp =>
          op match {
            case ConstantOp(result, value) =>
            case op: FIRRTLVerifOp =>
              op match {
                case AssertOp(clock, predicate, enable, message) =>
                case AssumeOp(clock, predicate, enable, message) =>
                case CoverOp(clock, predicate, enable, message)  =>
                case _                                           =>
              }
            case InvalidValueOp(result) =>
            case op: PrimOp =>
            case SpecialConstantOp(result, value)      =>
            case SubaccessOp(result, input, index)     =>
            case SubfieldOp(result, input, fieldIndex) =>
            case SubindexOp(result, input, index)      =>
            case _                                     =>
          }
        case op: ops.FIRRTLOp =>
          op match {
            case CircuitOp(name, body) =>
              writeLine(s"firrtl.circuit $name {")
              body.foreach(write(_)(b, indentNum + 1))
              writeLine("}")
            case FModuleOp(name, ports, body) =>
              writeLine(s"firrtl.module @$name (")
              ports.foreach(write(_)(b, indentNum + 1))
              writeLine(") {")
              body.foreach(write)
              writeLine("}")
            case FExtModuleOp(name, ports) =>
              writeLine(s"firrtl.module @$name {")
              ports.foreach(write(_)(b, indentNum + 1))
              writeLine(")")
            case AttachOp(arguments) =>
              write("firrtl.attach ")
              write(arguments.map(a => s"%${a._1}").mkString(", "))
              write(" : ")
              write(arguments.map(a => serialize(a._2)).mkString(", "))
              newLine()
            case ConnectOp(dest, src) =>
              writeLine(s"firrtl.connect %${dest._1}, %${src._1} : ${serialize(dest._2)}, ${serialize(src._2)}")
              newLine()
            case PartialConnectOp(dest, src) =>
              writeLine(s"firrtl.partialconnect ${dest._1}, ${src._1} : ${serialize(dest._2)}, ${serialize(src._2)}")
            case InstanceOp(results, instanceName, moduleName) =>
              // instance need module.
              writeLine(s"${results.map(_._1).mkString(", ")} = firrtl.instance $instanceName @$moduleName(${results
                .map(_._2)
                .map(p =>
                  s"${p.direction match {
                    case ops.In  => "in"
                    case ops.Out => "out"
                  }} ${p.name} ${serialize(p.tpe)}"
                )})")

            case WireOp(result) =>
              writeLine(s"${result._1} = firrtl.wire : ${serialize(result._2)}")
            case RegOp(result, clock) =>
              writeLine(s"${result._1} = firrtl.reg ${clock._1} : ${serialize(result._2)}")
            case RegResetOp(result, clock, reset, init) =>
              writeLine(
                s"${result._1} = firrtl.regreset ${clock._1}, ${reset._1}, ${init._1} : ${serialize(
                  reset._2
                )}, ${serialize(init._2)} ${serialize(result._2)}"
              )
            case SkipOp() =>
              writeLine("firrtl.skip")
            case StopOp(clock, cond, exitCode, name) =>
              writeLine(s"firrtl.stop ${clock._1}, ${cond._1}, $exitCode : $name")
            case PrintFOp(clock, cond, operands, formatSting) =>
              // TODO
            case NodeOp(result) =>
              // TODO: fix
              writeLine(s"${result._1} = ${serialize(result._2)}")
            case WhenOp(condition, thenRegion, elseRegion) =>
              writeLine(s"firrtl.when $condition {")
              thenRegion.foreach(write(_)(b, indentNum + 1))
              write("}")
              if (elseRegion.nonEmpty){
                write(" else {")
                elseRegion.foreach(write(_)(b, indentNum + 1))
                write("}")
              }
              newLine()
            case _                                         =>
          }
        case _ =>
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
    case op: ops.PortInfo =>
    case _ =>
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
