//package dspTools.firrtlPasses
//
//import firrtl.passes.Pass
//import firrtl.ir._
//import firrtl.Mappers._
//import firrtl.Annotations._
//
//class FPModifyDeclarations(componentTypes: Map[ComponentName, FixedType]) extends Pass {
//  def name = "Fixed Point Infer Types"
//  def run(c: Circuit): Circuit = {
//    def onModule(m: DefModule): DefModule = {
//      def getOrElse(name: String, tpe: Type) = {
//        val mname = ModuleName(m.name, CircuitName(c.main))
//        componentTypes.getOrElse(ComponentName(name, mname), tpe)
//      }
//      def onStmt(s: Statement): Statement = s match {
//        case DefWire(info, name, tpe) => DefWire(info, name, getOrElse(name, tpe))
//        case DefRegister(info, name, tpe, clock, reset, init) =>
//          val nType = getOrElse(name, tpe)
//          DefRegister(info, name, nType, clock, reset, init)
//        case _ => s map onStmt _
//      }
//      def onPorts(p: Port): Port = Port(p.info, p.name, p.direction, getOrElse(p.name, p.tpe))
//      m match {
//        case Module(info, name, ports, body) => Module(info, name, ports map onPorts, onStmt(body))
//        case ExtModule(info, name, ports) => ExtModule(info, name, ports map onPorts)
//      }
//    }
//    val newModules = c.modules.map{ onModule(_) }
//    Circuit(c.info, newModules, c.main)
//  }
//}
//
//// vim: set ts=4 sw=4 et:
