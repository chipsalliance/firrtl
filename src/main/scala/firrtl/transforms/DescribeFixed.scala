// See LICENSE for license details.

package firrtl.transforms

import firrtl._
import firrtl.annotations._
import firrtl.ir._
import firrtl.options.{Dependency, PreservesAll}

/**
 * Adds description to fixed point nodes giving its width and binary point
 */
class DescribeFixed extends Transform with PreservesAll[Transform] {
  def inputForm = UnknownForm
  def outputForm = UnknownForm

  override val prerequisites = Seq(Dependency[passes.InferBinaryPoints])

  override val dependents = Seq(Dependency(passes.ConvertFixedToSInt))

  private def onModule(cir: CircuitTarget)(m: DefModule): Seq[DescriptionAnnotation] = {
    val mod = ModuleTarget(circuit = cir.circuit, module = m.name)
    var descs = Seq[DescriptionAnnotation]()
    def onType(n: Target)(tpe: Type): Type = {
      tpe match {
        case FixedType(IntWidth(w), IntWidth(p)) =>
          descs = descs :+ DescriptionAnnotation(n, s"Fixed point with binary point $p")
        case f: FixedType => throw new Exception(s"Found unexpected fixed point number with unresolved binary point")
        case _ =>
      }
      tpe
    }
    def onStmt(s: Statement): Unit = s match {
      case b: Block => b.foreachStmt(onStmt)
      case ns: Statement with IsDeclaration =>
        val comp = ReferenceTarget(mod.circuit, mod.module, Nil, ns.name, Nil)
        ns.mapType(onType(comp))
      case _ =>
    }
    def onPort(p: Port): Unit = {
      val comp = ReferenceTarget(mod.circuit, mod.module, Nil, p.name, Nil)
      onType(comp)(p.tpe)
    }
    m.foreachStmt(onStmt)
    m.foreachPort(onPort)
    descs
  }

  def execute(state: CircuitState): CircuitState = {
    val descriptions = state.circuit.modules.flatMap(onModule(CircuitTarget(state.circuit.main)))
    state.copy(annotations = state.annotations ++ descriptions)
  }
}


