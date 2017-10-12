package coreir.ir

/* Namespace={
 * "namedtypes"? : {<name>: NamedType, ...}
 * "namedtypegens"? : {<name>: NamedTypeGen, ...}
 * "modules"? :{<name>:Module, ...},
 * "generators"? :{<name>:Generator, ...}
 * }
 */
case class Namespace(namedTypes: Option[Map[String, NamedType]],
                namedTypeGens: Option[Map[String, NamedTypeGen]],
                modules: Option[Map[String, Module]],
                generators: Option[Map[String, Generator]])

/* Type = "BitIn"
 *      | "Bit"
 *      | ["Array", <N>, Type]
 *      | ["Record", {<field>:Type,...} ]
 *      | ["Named",NamedRef, Args?]
 */
class Type ()
case class Bit() extends Type
case class BitIn() extends Type
case class Array(size: Int, tpe: Type) extends Type
case class Record(fields: Map[String, Type]) extends Type
case class Named(named: NamedRef, args: Option[Seq[Arg]]) extends Type


/* //This could be referring a type, module, or generator
 * NamedRef = "<namespaceName>.<name>"
 * NamedType = {"flippedname":<name>,"rawtype":Type}
 * NamedTypeGen = {"flippedname"?:<name>,"genparams":Parameter}
 */
case class NamedRef(namespaceName: String, name: String)
case class NamedType(flippedName: String, rawtype: Type)
case class NamedTypeGen(flippedName: Option[String], genparams: Parameters)

/* Note if there are no instances and no connections, this is a declaration
 * Module = {
 * "type":Type,
 * "modparams"?:Parameter,
 * "defaultmodargs"?:Values,
 * "instances"?:{<instname>:Instance,...},
 * "connections"?: Connection[]
 * }
 */
case class Module(tpe: Type,
                  modParams: Option[Parameters],
                  defaultModArgs: Option[Seq[Value]],
                  instances: Option[Map[String, Instance]],
                  connections: Option[Seq[Connection]])

/* Generator = {
 * "typegen":NamedRef
 * "genparams":Parameters,
 * "defaultgenargs"?:Consts,
 * }
 */
case class Generator(typegen: NamedRef,
                     genparams: Parameters,
                     defaultGenArgs: Option[Seq[Const]])

/*
 * Instance = {
 * "genref"?:NamedRef,
 * "genargs"?:Values,
 * "modref"?:NamedRef,
 * "modargs"?:Values
 * }
 */
case class Instance(genRef: Option[NamedRef],
                    genArgs: Option[Seq[Value]],
                    modRef: Option[NamedRef],
                    modArgs: Option[Seq[Value]])

/*
Connection = [Wireable, Wireable]
*/
case class Connection(w0: Wireable, w1: Wireable)

/* accesses instname.a.b If "instname" is "self" then this is the module's interface.
 * Note: a,b can be digits representing an index.
 * Wireable = "<instname>,<a>,<b>,..."
*/
case class Wireable(names: Seq[String])


/*
//The following is my Value IR.
//This contains a small IR representing constants and Referneces to generator/module args. This will be expanded

ValueType = "Bool"
| "Int"
| ["BitVector" <N>]
| "String"
| "CoreIRType"
*/
class ValueType()
case class ValueBool() extends ValueType
case class ValueInt() extends ValueType
case class ValueBitVector(size: Int) extends ValueType
case class ValueString() extends ValueType
case class CoreIRType() extends ValueType

/* Params = {<field>:ValueType,...} */
case class Parameters(params: Map[String, ValueType])

/* Value = Arg
 *       | Const
 *
 * Arg = [ValueType, "Arg", <field>]
 *
 * Const = [ValueType, <Val>]
 */
class Value()
case class Arg(valueType: ValueType, field: String) extends Value
case class Const(valueType: ValueType, value: String) extends Value

/* Values = {<field>:Value,...} */
case class Values(values: Map[String, Value])
