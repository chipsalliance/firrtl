//package dspTools.firrtlPasses
//import scala.collection.mutable
//import firrtl.passes.Pass
//import firrtl.ir._
//import firrtl.Mappers._
//import firrtl.{WGeq, VarWidth}
//
//object FPInferWidths extends Pass {
//  def name = "Infer Fixed Type Widths"
//  def run (c:Circuit): Circuit = {
//    val v = mutable.ArrayBuffer[WGeq]()
//    def constrain(t1: Type, t2: Type) : Unit = (t1, t2) match {
//      case (FixedType(w1, p1), FixedType(w2, p2)) => 
//        v += WGeq(w1,w2)
//        v += WGeq(p1,p2)
//      case _ =>
//    }
//    def collectConstraints(s: Statement): Statement = (s map collectConstraints) match {
//      case Connect(info, loc, exp) =>
//        constrain(loc.tpe,exp.tpe)
//        s
//      case DefRegister(info, _, tpe, _, _, init) =>
//        constrain(tpe,init.tpe)
//        s
//      case (s) => s map (collectConstraints)
//    }
//    def insertTypeWidths(h: Map[String, Width])(tpe: Type): Type = {
//      def onWidth(w: Width): Width = w match {
//        case VarWidth(n) => firrtl.passes.InferWidths.evaluate(h.getOrElse(n, UnknownWidth), h)
//        case i:IntWidth => i
//        case UnknownWidth => error("Shouldn't be here")
//      }
//      tpe match {
//        case FixedType(w, p) => FixedType(onWidth(w), onWidth(p))
//        case _ => tpe map insertTypeWidths(h)
//      }
//    }
//    def insertStmtWidths(h: Map[String, Width])(s: Statement): Statement =
//      (s map insertTypeWidths(h)) map insertStmtWidths(h)
//    def insertPortWidths(h: Map[String, Width])(p: Port): Port =
//      Port(p.info, p.name, p.direction, insertTypeWidths(h)(p.tpe))
//
//    for (m <- c.modules) { m map collectConstraints }
//
//    /*
//    println-debug("======== ALL CONSTRAINTS ========")
//    for x in v do : println-debug(x)
//    println-debug("=================================")
//    */
//
//    val h = firrtl.passes.InferWidths.solve_constraints(v).toMap
//
//    /*
//    println("======== SOLVED CONSTRAINTS ========")
//    for(x <- h) { println(x) }
//    println("====================================")
//    */
//
//    val newModules = for (m <- c.modules) yield {
//       m map insertPortWidths(h) map insertStmtWidths(h)
//    }
//    FPInferTypes.run(Circuit(c.info, newModules, c.main))
//  }
//}
//
//// vim: set ts=4 sw=4 et:
