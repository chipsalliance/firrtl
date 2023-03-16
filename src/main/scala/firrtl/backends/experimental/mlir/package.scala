// SPDX-License-Identifier: Apache-2.0
// Author: Jiuyang Liu <liu@jiuyang.me>

package firrtl.backends.experimental

package object mlir {

  /**
    * FIRRTL Enum Definitions
    * In Scala we define this with a trait(thanks pattern match in Scala)
    *
    * I32EnumAttrCase in C++
    */
  trait Enum {
    def name:  String
    def value: Int
  }


  // type alias
  type unsigned = BigInt
  def canCastToUnsigned(unsigned: BigInt) = require(unsigned >= 0, s"negative BigInt $unsigned cannot be cast to unsigned")
  type I32Attr = BigInt
  type StrAttr = String
  type BoolAttr = Boolean
  type StringRef = String
  type SymbolNameAttr = String
  type Location = String
  type AnyType = String
  type AnnotationArrayAttr = Seq[String]
  type AnnotationSet = Seq[String]
  type NameRefArrayAttr = Seq[String]
  type ArrayAttr = Seq[String]
  type ParamDeclArrayAttr = Seq[String]
  type StringAttr = String
  type OptionalAttr[T] = Option[T]
  type Variadic[T] = Seq[T]
  type ArrayRef[T] = Seq[T]
  // hack
  type DefaultValuedAttr[T] = T
}
