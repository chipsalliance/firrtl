package firrtl.form

import firrtl.ir.Circuit

import scala.collection.mutable

/*
  I want to easily say do these transformations in sequence
  I don't want to have to define a new Feature for each transform

 */

case class CircuitState(circuit: Circuit, form: Form)

case class Form(valids: Set[Feature], invalids: Set[Feature]) {
  require(valids.intersect(invalids).isEmpty, s"Invalid form: valids $valids and invalids $invalids intersect.")
  def contains(other: Form): Boolean = {
    other.valids.forall { valids.contains }
    other.invalids.forall { invalids.contains }
  }
  def update(other: Form): Form = {
    val newValids = valids.diff(other.invalids) ++ other.valids
    val newInvalids = invalids.diff(other.valids) ++ other.invalids
    Form(newValids, newInvalids)
  }
  def remove(other: Form): Form = {
    Form(valids.diff(other.valids), invalids.diff(other.invalids))
  }
  def withInvalid(features: Feature*): Form = Form(valids, invalids ++ features)
  def withValid(features: Feature*): Form = Form(valids ++ features, invalids)
}

object Form {
  def apply(): Form = Form(Set.empty[Feature], Set.empty[Feature])
  def apply(forms: Form*): Form = forms.foldLeft(Form())((curr, next) => curr.update(next))
  implicit def feature2form(f: Feature): Form = f match {
    case NotFeature(x) => Form(Set.empty[Feature], Set(x))
    case x => Form(Set(x), Set.empty[Feature])
  }
  implicit def transform2form(t: Transform): Form = t.contributes
  //def apply(features: Feature*): Form = Form(features.toSet, Set.empty[Feature])
  //def apply(xforms: Seq[Transform]): Form = xforms.foldLeft(Form())((f, x) => f update x.contributes)
}

trait Feature {
  def description: String
  def checkValid(cs: CircuitState): Unit
  def checkInvalid(cs: CircuitState): Unit
}
case class NotFeature(f: Feature) extends Feature {
  def description: String = "Invalid: " + f.description
  def checkValid(cs: CircuitState): Unit = f.checkInvalid(cs)
  def checkInvalid(cs: CircuitState): Unit = f.checkValid(cs)
}

case class SimpleFeature(cls: Class[_<:SimpleTransform], description: String, checkFun: CircuitState=>Unit) extends Feature {
  def checkValid(cs: CircuitState): Unit = checkFun(cs)
  def checkInvalid(cs: CircuitState): Unit = {}
}

object Feature {
  def apply(xform: SimpleTransform): Feature = SimpleFeature(xform.getClass, xform.description, xform.check)
}

trait Transform {
  /* If you run me, I require my input state to contain this form */
  def requires: Form
  /* If you run me, I'll contribute this to your Form */
  val contributes: Form
  /* If you run me, the end result better have my goal */
  def future: Form
  /* Run me */
  def execute(cs: CircuitState): CircuitState
}

trait SimpleTransform extends Transform {
  def description: String
  def feature = Feature(this)
  val contributes = Form(feature)
  val future = Form()
  def check(cs: CircuitState): Unit
  def checkValid(cs: CircuitState): Unit = check(cs)
  def checkInvalid(cs: CircuitState): Unit = {}
}

trait MultiTransform extends Transform {
  def orderedTransforms: Seq[Transform]

  /* Run checks on state after each transform */
  def runFeatureChecks: Boolean

  def execute(cs: CircuitState): CircuitState = {
    if(runFeatureChecks) {
      cs.form.valids.foreach { v => v.checkValid(cs) }
      cs.form.invalids.foreach { i => i.checkInvalid(cs) }
    }
    orderedTransforms.foldLeft(cs){ (x, t) =>

      /* Check required form */
      require(cs.form.contains(t.requires), s"CircuitState of form ${cs.form} failed requirement ${t.requires} of transform $t.")

      /* Execute transform */
      val result = t.execute(x)

      /* Check result */
      if(runFeatureChecks) {
        result.form.valids.foreach { v => v.checkValid(result) }
        result.form.invalids.foreach { i => i.checkInvalid(result) }
      }

      result
    }
  }
  val contributes: Form = orderedTransforms.foldLeft(Form())((f, t) => f.update(t.contributes))
  val requires: Form = orderedTransforms.foldRight(Form())((t, f) => f.remove(t.contributes).update(t.requires))
  val future: Form = orderedTransforms.foldRight(Form())((t, f) => f.update(t.future))
}

trait SeqTransform extends MultiTransform {
  def order(transforms: Iterable[Transform]): Seq[Transform] = transforms.toSeq
}

object SequencedTransform {
  def apply(trans: Iterable[Transform]): SeqTransform = new SeqTransform {
    def orderedTransforms: Seq[Transform] = trans.toSeq
    def runFeatureChecks: Boolean = false
  }
}

trait ScheduledTransform extends MultiTransform {
  def order(transforms: Iterable[Transform], goal: Form): Seq[Transform] = {
    val graph = new firrtl.graph.MutableDiGraph[Form]()
    graph.addVertex(requires)

    /* Contains current state and current goal */
    val stateQueue = mutable.ArrayBuffer[(Form, Form)]()
    val transformMap = mutable.HashMap[(Form, Form), Transform]()
    val goalMap = mutable.HashMap[(Form, Form), Form]()

    var nTransforms = 0
    var currForm = requires
    var currGoal = goal

    while(!currForm.contains(currGoal) && nTransforms < 100) {
      transforms.foreach { t =>
        val nextForm = currForm.update(t.contributes)
        val nextGoal = currGoal.update(t.future)

        if(currForm.contains(t.requires) && !graph.contains(nextForm)) {
          graph.addVertex(nextForm)
          graph.addEdge(currForm, nextForm)
          transformMap((currForm, nextForm)) = t
          stateQueue += ((nextForm, nextGoal))
        }
      }
      nTransforms += 1
      if(stateQueue.nonEmpty) {
        val tup = stateQueue.remove(0)
        currForm = tup._1
        currGoal = tup._2
      } else {
        nTransforms = 1000
      }
    }

    if(nTransforms < 100) {
      val path = graph.path(requires, currForm)
      //println(path.mkString("\n"))
      val (lastForm, orderedxforms) = path.tail.foldLeft((path.head, Seq.empty[Transform])) {
        case ((prevForm, xforms), nextForm) =>
          (nextForm, xforms :+ transformMap((prevForm, nextForm)))
      }
      orderedxforms
    } else Nil
  }
}

object ScheduledTransform {
  def apply(trans: Iterable[Transform], goal: Form): ScheduledTransform = new ScheduledTransform {
    def orderedTransforms: Seq[Transform] = order(trans, goal)

    override def runFeatureChecks: Boolean = false
  }
}

trait FakeTransform extends SimpleTransform {
  def execute(cs: CircuitState): CircuitState = cs
  def check(cs: CircuitState): Unit = {}
  def description = "Fake news!"
}

class IRTransform extends FakeTransform        { val requires = Form(); override val contributes = Form(feature).withInvalid(new WIRTransform().feature) }
class WIRTransform extends FakeTransform       { val requires = Form(); override val contributes = Form(feature).withInvalid(new IRTransform().feature) }
class TypesTransform extends FakeTransform     { val requires = Form(new WIRTransform, new KindsTransform) }
class GendersTransform extends FakeTransform   { val requires = Form(new WIRTransform, new KindsTransform) }
class KindsTransform extends FakeTransform     { val requires = Form(new WIRTransform) }
class WidthsTransform extends FakeTransform    { val requires = Form(new WIRTransform, new TypesTransform, new KindsTransform, new GendersTransform) }
class EmitVerilog extends FakeTransform        { val requires = Form(Resolved) }
// This would do nothing, a way to convert between equivalent feature sets
case object Resolved extends Feature {
  def description: String = "Resolved!"
  def checkValid(cs: CircuitState): Unit = {}
  def checkInvalid(cs: CircuitState): Unit = {}
}
import Form.transform2form
class ResolveTransform extends SeqTransform {
  override val contributes: Form = orderedTransforms.foldLeft(Form())((f, t) => f.update(t.contributes)).withValid(Resolved)
  def orderedTransforms = Seq(new WIRTransform, new TypesTransform, new GendersTransform, new KindsTransform, new WidthsTransform)
  override def runFeatureChecks: Boolean = false
}

class VerilogCompiler extends ScheduledTransform {
  def orderedTransforms = order(Seq(new WIRTransform, new TypesTransform, new GendersTransform, new KindsTransform, new WidthsTransform, new EmitVerilog), new EmitVerilog().contributes)

  override def runFeatureChecks: Boolean = false
}

object Runner extends App {
  val cs = CircuitState(Circuit(firrtl.ir.NoInfo, Nil, "top"), Form())
  val result = new VerilogCompiler().execute(cs)
  println("Done")
}
