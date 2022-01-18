// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

// This files is based on circt/include/circt/Dialect/HW/???.td

package firrtl.backends.experimental.mlir.ops

import firrtl.backends.experimental.mlir._

/** StrAttr:$name
  * DefaultValuedAttr<AnnotationArrayAttr, "{}">:$annotations
  *
  * SizedRegion<1>:$body
  */
case class CircuitOp(name: StrAttr, annotations: DefaultValuedAttr[AnnotationArrayAttr], body: Block) extends FIRRTLOp

/** "StringAttr":$name
  * "ArrayRef<PortInfo>":$ports
  * CArg<"ArrayAttr","ArrayAttr()">:$annotations)
  *
  * SizedRegion<1>:$body
  */
case class FModuleOp(name: StringAttr, ports: ArrayRef[PortInfo], body: Block) extends FIRRTLOp
/** OptionalAttr<StrAttr>:$defname
  * ParamDeclArrayAttr:$parameters
  * DefaultValuedAttr<AnnotationArrayAttr, "ArrayAttr()">:$annotations
  */
case class FExtModuleOp(defname: OptionalAttr[StrAttr], parameters: ParamDeclArrayAttr, annotations: DefaultValuedAttr[AnnotationArrayAttr], body: Block) extends FIRRTLOp
/**
  * SymbolNameAttr:$sym_name
  * NameRefArrayAttr:$namepath
  */
case class NonLocalAnchor(sym_name: SymbolNameAttr, namepath: NameRefArrayAttr) extends FIRRTLOp