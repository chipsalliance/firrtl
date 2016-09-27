//package dspTools.firrtlPasses
//
//import firrtl._
//import firrtl.ir._
//import firrtl.Annotations._
//import firrtl.passes.{PassException, Pass}
//import firrtl.Mappers._
//import scala.collection.mutable
//
//case class WrongTypeException(x: Annotation) extends PassException(s"Annotation $x must be a FixedTypeAnnotation.")
//case class WrongNameException(x: Named) extends PassException(s"Annotation's name, $x, must be a ComponentName.")
//case class NoExModuleException(x: String) extends PassException(s"Annotated module $x cannot be an external module.")
//case class IllegalComponentException(k: ComponentName) extends PassException(s"Annotated component cannot be annotated: ${k.module.name}.${k.name}")
//case class MissingComponentException(k: ComponentName) extends PassException(s"Annotated component does not exist: ${k.module.name}.${k.name}")
//
//// Only on Low Firrtl!
//class FixedPointTransform (transID: TransID) extends Transform {
//  def name = "Fixed Point Transform"
//  def execute(circuit: Circuit, annotationMap: AnnotationMap): TransformResult = {
//    annotationMap.get(transID) match {
//      case None => TransformResult(circuit, None, None)
//      case Some(map) => {
//        val componentTypes = mutable.HashMap[ComponentName, FixedType]()
//        map.values.foreach {x: Annotation => x match {
//          case FixedTypeAnnotation(ComponentName(com, m), _, tpe)  => componentTypes(ComponentName(com, m)) = tpe
//          case FixedTypeAnnotation(x, _, tpe)  => throw WrongNameException(x)
//          case _ => throw WrongTypeException(x)
//        }}
//        check(circuit, componentTypes.toMap)
//        val passes = Seq(
//          new FPModifyDeclarations(componentTypes.toMap),
//          FPInferTypes,
//          FPInferWidths,
//          FPConvertToSInt,
//          firrtl.passes.RemoveEmpty
//        )
//        val circuitResult = passes.foldLeft(circuit)((in: Circuit, pass: Pass) => pass.run(in))
//        TransformResult(circuitResult, None, None)
//      }
//    }
//  }
//
//  def check(c: Circuit, componentTypes: Map[ComponentName, FixedType]): Unit = {
//    val errors = new firrtl.passes.Errors()
//    val moduleMap = (for(m <- c.modules) yield m.name -> m).toMap
//    val containsCN = mutable.HashMap[ComponentName, Option[Boolean]]()
//    containsCN ++= componentTypes.map(x => (x._1, None))
//
//    val cname = CircuitName(c.main)
//    def has(n: String, m: String): Boolean = containsCN.contains(ComponentName(n, ModuleName(m, cname)))
//    def set(n: String, m: String): Unit = containsCN(ComponentName(n, ModuleName(m, cname))) = Some(true)
//    def nset(n: String, m: String): Unit = containsCN(ComponentName(n, ModuleName(m, cname))) = Some(false)
//    def checkBody(s: Statement, mname: String): Unit = {
//      def onStmt(s: Statement): Statement = {
//        s match {
//          case DefRegister(_,n,_,_,_,_) if (has(n, mname)) => set(n, mname)
//          case DefWire(_,n,_) if (has(n, mname)) => set(n, mname)
//          case DefNode(_,n,_) if (has(n, mname)) => set(n, mname)
//          case DefMemory(_,n,_,_,_,_,_,_,_,_) if (has(n, mname)) => nset(n, mname)
//          case _ =>
//        }
//        s map onStmt
//      }
//      onStmt(s)
//    }
//    def checkPorts(ports: Seq[Port], mname: String): Unit = {
//      for(p <- ports) if(has(p.name, mname)) set(p.name, mname)
//    }
//    for(m <- c.modules) { m match {
//      case Module(info, name, ports, body) =>
//        checkBody(body, name)
//        checkPorts(ports, name)
//      case ExtModule(info, name, ports) =>
//        checkPorts(ports, name)
//        errors.append(new NoExModuleException(name))
//    }}
//    for((k, v) <- containsCN) { v match {
//      case Some(true) => //contains and is legal
//      case Some(false) => //contains and is illegal
//        errors.append(IllegalComponentException(k))
//      case None => //not found
//        errors.append(MissingComponentException(k))
//    }}
//    errors.trigger
//  }
//}
//
//// vim: set ts=4 sw=4 et:
