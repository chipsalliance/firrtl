package coreir.json

import coreir.ir._
import play.api.libs.json.Json._
import play.api.libs.json._

import scala.collection.mutable

object CoreIRWrites {
  implicit  val TopWrites: Writes[Top] = new Writes[Top] {
    def writes(o: Top) = JsObject(
      Seq(
        ("top" -> NamedRefWrites.writes(o.top)),
        ("namespaces" -> JsObject(o.namespaces.map(x => (x._1 -> NamespaceWrites.writes(x._2)))))
      )
    )
  }

  implicit  val NamespaceWrites: Writes[Namespace] = new Writes[Namespace] {
    def writes(ns: Namespace) = {
      val list = mutable.ArrayBuffer[(String, JsValue)]()
      if(ns.namedTypes.isDefined) list += ("namedtypes" -> JsObject(ns.namedTypes.get.map(x => (x._1, NamedTypeWrites.writes(x._2)))))
      if(ns.namedTypeGens.isDefined) list += ("namedtypegens" -> JsObject(ns.namedTypeGens.get.map(x => (x._1, NamedTypeGenWrites.writes(x._2)))))
      if(ns.modules.isDefined) list += ("modules" -> JsObject(ns.modules.get.map(x => (x._1, ModuleWrites.writes(x._2)))))
      if(ns.generators.isDefined) list += ("generators" -> JsObject(ns.generators.get.map(x => (x._1, GeneratorWrites.writes(x._2)))))
      JsObject(list.toSeq)
    }
  }

  implicit  val AllTypeWrites: Writes[Type] = new Writes[Type] {
    def writes(tpe: Type) = tpe match {
      case t: Bit => BitWrites.writes(t)
      case t: BitIn => BitInWrites.writes(t)
      case t: Array => ArrayWrites.writes(t)
      case t: Record => RecordWrites.writes(t)
      case t: Named => NamedWrites.writes(t)
      //case _ => throw new RuntimeException("Unsupported Type.")
    }
  }

  implicit  val BitWrites:Writes[Bit] = new Writes[Bit] {
    def writes(t: Bit) = JsString("Bit")
  }

  implicit  val BitInWrites:Writes[BitIn] = new Writes[BitIn] {
    def writes(t: BitIn) = JsString("BitIn")
  }

  implicit  val ArrayWrites:Writes[Array] = new Writes[Array] {
    def writes(t: Array) = Json.arr(JsString("Array"), JsNumber(t.size), AllTypeWrites.writes(t.tpe))
  }

  implicit  val RecordWrites:Writes[Record] = new Writes[Record] {
    def writes(t: Record) = {
      val JsFields = t.fields.map { x => (x._1, AllTypeWrites.writes(x._2)) }.toSeq
      Json.arr(JsString("Record"), JsObject(JsFields) )
    }
  }

  implicit  val NamedWrites:Writes[Named] = new Writes[Named] {
    def writes(t: Named) = {
       // ["Named",NamedRef, Args?]
      val list = mutable.ArrayBuffer[JsValue]()
      list += JsString("Named")
      list += NamedRefWrites.writes(t.named)
      if(t.args.isDefined) list ++= t.args.get.map(ArgWrites.writes)
      JsArray(list)
    }
  }

  implicit  val NamedRefWrites: Writes[NamedRef] = new Writes[NamedRef] {
    def writes(t: NamedRef) = JsString(t.namespaceName + "." + t.name)
  }

  implicit  val NamedTypeWrites: Writes[NamedType] = new Writes[NamedType] {
    def writes(t: NamedType) = JsObject(
      Map("flippedname" -> JsString(t.flippedName), "rawtype" -> AllTypeWrites.writes(t.rawtype))
    )
  }

  implicit  val NamedTypeGenWrites: Writes[NamedTypeGen] = new Writes[NamedTypeGen] {
    def writes(t: NamedTypeGen) = {
      val stuff = t.flippedName.map(s => Seq(("flippedname" -> JsString(s)))).getOrElse(Nil) ++
        Seq("genparams" -> ParamsWrites.writes(t.genparams))
      JsObject(stuff)
    }
  }

  implicit  val ModuleWrites: Writes[Module] = new Writes[Module] {
    def writes(t: Module) = {
      val list = mutable.ArrayBuffer[(String, JsValue)]()
      list += ("type" -> AllTypeWrites.writes(t.tpe))
      if(t.modParams.isDefined) list += ("modparams" -> ParamsWrites.writes(t.modParams.get))
      if(t.defaultModArgs.isDefined) list += ("defaultmodargs" -> ValuesWrites.writes(t.defaultModArgs.get))
      if(t.instances.isDefined) list += ("instances" -> JsObject(t.instances.get.map(x => (x._1, InstanceWrites.writes(x._2)))))
      if(t.connections.isDefined) list += ("connections" -> JsArray(t.connections.get.map(x => ConnectionWrites.writes(x))))
      JsObject(list.toSeq)
    }
  }

  implicit  val GeneratorWrites: Writes[Generator] = new Writes[Generator] {
    def writes(t: Generator) = {
      val list = mutable.ArrayBuffer[(String, JsValue)]()
      list += ("typegen" -> NamedRefWrites.writes(t.typegen))
      list += ("genparams" -> ParamsWrites.writes(t.genparams))
      if(t.defaultGenArgs.isDefined) list += ("defaultgenargs" -> JsArray(t.defaultGenArgs.get.map(ConstWrites.writes(_))))
      JsObject(list.toSeq)
    }
  }

  implicit  val InstanceWrites: Writes[Instance] = new Writes[Instance] {
    def writes(t: Instance) = {
      val list = mutable.ArrayBuffer[(String, JsValue)]()
      if(t.genRef.isDefined) list += ("genref" -> NamedRefWrites.writes(t.genRef.get))
      if(t.genArgs.isDefined) list += ("genargs" -> ValuesWrites.writes(t.genArgs.get))
      if(t.modRef.isDefined) list += ("modref" -> NamedRefWrites.writes(t.modRef.get))
      if(t.modArgs.isDefined) list += ("modargs" -> ValuesWrites.writes(t.modArgs.get))
      JsObject(list.toSeq)
    }
  }

  implicit  val ConnectionWrites: Writes[Connection] = new Writes[Connection] {
    def writes(t: Connection) = {
      val list = mutable.ArrayBuffer[JsValue]()
      list += WireableWrites.writes(t.w0)
      list += WireableWrites.writes(t.w1)
      JsArray(list.toSeq)
    }
  }

  implicit  val WireableWrites: Writes[Wireable] = new Writes[Wireable] {
    def writes(t: Wireable) = JsString(t.names.mkString("."))
  }

  implicit  val ValueTypeWrites: Writes[ValueType] = new Writes[ValueType] {
    def writes(tpe: ValueType) = tpe match {
      case t: ValueInt => ValueIntWrites.writes(t)
      case t: ValueBitVector => ValueBitVectorWrites.writes(t)
      case t: ValueString => ValueStringWrites.writes(t)
      case t: ValueBool => ValueBoolWrites.writes(t)
      case t: CoreIRType => CoreIRTypeWrites.writes(t)
      //case _ => throw new RuntimeException("Unsupported Type.")
    }
  }

  implicit  val ValueBoolWrites: Writes[ValueBool] = new Writes[ValueBool] {
    def writes(tpe: ValueBool) = JsString("Bool")
  }

  implicit  val ValueIntWrites: Writes[ValueInt] = new Writes[ValueInt] {
    def writes(tpe: ValueInt) = JsString("Int")
  }

  implicit  val ValueBitVectorWrites: Writes[ValueBitVector] = new Writes[ValueBitVector] {
    def writes(tpe: ValueBitVector) = JsArray(Seq(JsString("BitVector"), JsNumber(tpe.size)))
  }

  implicit  val ValueStringWrites: Writes[ValueString] = new Writes[ValueString] {
    def writes(tpe: ValueString) = JsString("String")
  }

  implicit  val CoreIRTypeWrites: Writes[CoreIRType] = new Writes[CoreIRType] {
    def writes(tpe: CoreIRType) = JsString("CoreIRType")
  }

  implicit  val ParamsWrites: Writes[Parameters] = new Writes[Parameters] {
    def writes(o: Parameters) = {
      JsObject(o.params.map(x => (x._1, ValueTypeWrites.writes(x._2))))
    }
  }

  /*
  implicit  val ValueWrites: Writes[Value] = new Writes[Value] {
    def writes(o: Value) = {
      val value = o.valueType match {
        case t: ValueInt => JsNumber(o.value.toInt)
        case t: ValueString => JsString(o.value)
        case _ => error("Unsupported valuetype?")
      }

      JsArray(Seq(ValueTypeWrites.writes(o.valueType), value))
    }
  }
  */

  implicit  val ValueWrites: Writes[Value] = new Writes[Value] {
    def writes(o: Value) =  o match {
      case t: Arg => ArgWrites.writes(t)
      case t: Const => ConstWrites.writes(t)
    }
  }

  implicit  val ArgWrites: Writes[Arg] = new Writes[Arg] {
    def writes(o: Arg) = JsArray(Seq(ValueTypeWrites.writes(o.valueType), JsString("Arg"), JsString(o.field)))
  }

  implicit  val ConstWrites: Writes[Const] = new Writes[Const] {
    def writes(o: Const) = {
      val value = o.valueType match {
        case t: ValueInt => JsNumber(o.value.toInt)
        case t: ValueString => JsString(o.value)
        case t: ValueBitVector => JsNumber(o.value.toInt)
        case t: ValueBool => JsBoolean(o.value.toBoolean)
        case _ => error(s"Unsupported ${o.valueType}")
      }
      JsArray(Seq(ValueTypeWrites.writes(o.valueType), value))
    }
  }

  implicit  val ValuesWrites: Writes[Values] = new Writes[Values] {
    def writes(o: Values) = JsObject(o.values.map(x => (x._1 -> ValueWrites.writes(x._2))))
  }
}

object CoreIRJson extends App {
  import CoreIRWrites._
  val tpe = Record(Map("a" -> Array(16, Bit()), "b" -> Array(16, Bit())))
  val x = Json.toJson(tpe)
  println(x)
}

/*
case class Named(ref: NamedRef, args: Args)
case class Arg()
//Arg = [ValueType, "Arg", <field>]
case class NamedRef(namespaceName: String, name: String)
*/


/*
//Definitions

Namespace={
  "namedtypes"? : {<name>: NamedType, ...}
  "namedtypegens"? : {<name>: NamedTypeGen, ...}
  "modules"? :{<name>:Module, ...},
  "generators"? :{<name>:Generator, ...}
}

Type = "BitIn"
     | "Bit"
     | ["Array", <N>, Type]
     | ["Record", {<field>:Type,...} ]
     | ["Named",NamedRef, Args?]

//This could be referring a type, module, or generator
NamedRef = "<namespaceName>.<name>"

NamedType = {"flippedname":<name>,"rawtype":Type}
NamedTypeGen = {"flippedname"?:<name>,"genparams":Parameter}

//Note if there are no instances and no connections, this is a declaration
Module = {
  "type":Type,
  "modparams"?:Parameter,
  "defaultmodargs"?:Values,
  "instances"?:{<instname>:Instance,...},
  "connections"?: Connection[]
}

Generator = {
  "typegen":NamedRef
  "genparams":Parameters,
  "defaultgenargs"?:Consts,
}

Instance = {
  "genref"?:NamedRef,
  "genargs"?:Values,
  "modref"?:NamedRef,
  "modargs"?:Values
}

Connection = [Wireable, Wireable]

//accesses instname.a.b If "instname" is "self" then this is the module's interface.
//Note: a,b can be digits representing an index.
Wireable = "<instname>,<a>,<b>,..."


//The following is my Value IR.
//This contains a small IR representing constants and Referneces to generator/module args. This will be expanded

ValueType = "Bool"
          | "Int"
          | ["BitVector" <N>]
          | "String"
          | "CoreIRType"

Params = {<field>:ValueType,...}

Arg = [ValueType, "Arg", <field>]
Const = [ValueType, <Val>]

Value = Arg
      | Const

Values = {<field>:Value,...}
 */
