package firrtl.annotations.transforms

class EliminateReferenceSelectors {
  //val circuit = state.circuit
  //renameMap.setCircuit(circuit.main)

  //Should first resolve instance refs, Map(originalTargets -> Seq[nonHierarchicalTargets])
  // Could be a seq if top.x1.foo and X.bar are both targets, X.bar will point to X.bar and X_.bar
  // Maps (Original -> (Duped -> Instance, Duped))


  //Then, should resolve clock, init, regs, etc. Map(nonHierarchicalTargets -> Seq[finalTarget])



  /*
  def resolveBase(comp: Component, index: Int): Seq[Component] = {
    val refs = comp.reference.slice(index, comp.reference.length)
    def module: String = {
      refs.reverse.collectFirst{ case OfModule(m) => m }.getOrElse(comp.module.get)
    }
    refs match {
      case Nil => Seq(comp)
      case Instance(i) :: OfModule(m) :: tail if instanceMap(module)(i).module == m =>
        resolveBase(comp, index + 2)
      case Ref(r) :: Nil if declaresMap(module).contains(r) =>
        Seq(comp)
      case Regs :: Nil =>
        registerMap(module).values.map { dr =>
          val pre = Component(comp.circuit, comp.module, comp.reference.dropRight(1), comp.tag)
          pre.ref(dr.name)
        }.toSeq
      case other => Nil
    }
  }

  def resolveFields(reference: Seq[SubComponent]): Seq[Component] = ???

  def resolve(component: Component): Seq[Component] = component match {
    case Component(Some(`circuitName`), Some(mod), reference, _) if moduleMap.keySet.contains(mod) =>
      resolveBase(component, 0)
    case _ => Nil
  }
  */


}
