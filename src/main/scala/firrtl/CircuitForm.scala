package firrtl

trait Form {
  /* Children Forms */
  def elements: Set[Form]
  def contains(other: Form): Boolean = {
    validate()
    val set = baseElements.toSet
    other.baseElements.forall { set.contains }
  }
  def update(other: Form): Form = {
    validate()
    val fin = baseElements.foldLeft(other.baseElements) { (set, f) =>
      f match {
        case Not(x) if set.contains(x) => set
        case x if set.contains(Not(x)) => set
        case _ => set + f
      }
    }
    Form(fin)
  }
  def baseElements: Set[Form] = elements.flatMap(_.baseElements)
  def feature: Option[String]
  def featureList: Seq[String] = feature.map(Seq(_)).getOrElse(elements.flatMap(_.featureList)).toSeq
  def description: String =
    s"""Form: $this
       |Feature: $feature
       |Other Features:"
       |${featureList.map(" - " + _).mkString("\n")}"
       |"""".stripMargin

  /* Check that no form contains both a form and its inversion */
  def validate(): Unit = {
    val nots = baseElements.collect { case Not(x) => x }
    require(baseElements.forall(!nots.contains(_)))
  }
}
class Not(val form: Form) extends Form {
  def elements = Set(form)
  override def baseElements: Set[Form] = form.baseElements.map {
    case Not(x) => x
    case x => Not(x)
  }
  override def toString: String = s"Not($form)"
  override def hashCode: Int = {
    val prime = 31
    var result = 1
    prime * result + form.hashCode()
  }
  override def equals(obj: Any): Boolean = obj match {
    case Not(x) => x == form && this.hashCode == obj.hashCode
    case _ => false
  }
  def feature = None
  override def featureList: Seq[String] = form.featureList.map("Not: " + _).map {
    case s: String if s.startsWith("Not: Not: ") => s.drop(10)
  }
}
object Not {
  def apply(features: Form*): Form = if(features.size == 1) new Not(features.head) else GroupForm(features.map(new Not(_)).toSet)
  def unapply(f: Form): Option[Form] = f match {
    case f: Not => Some(f.form)
    case _ => None
  }
}

trait SingleForm extends Form {
  def elements = Set.empty[Form]
  override def baseElements: Set[Form] = Set(this)
}

case object UnknownForm extends Form {
  val elements = Set.empty[Form]
  def feature = Some("Unknown form.")
}

case class GroupForm(elements: Set[Form]) extends Form {
  def feature = None
}

object Form {
  def apply(elements: Form*): Form = GroupForm(elements.toSet)
  def apply(elements: Iterable[Form]): Form = GroupForm(elements.toSet)
}

/* Standard FIRRTL Forms */
case object ChirrtlForm extends SingleForm {
  def feature = Some("Default")
}
case object HighForm extends Form {
  def elements: Set[Form] = Set(ChirrtlForm)
  override def feature = Some("Does not contain cmem, smem, and mport")
}
case object MidForm extends SingleForm {
  override def elements: Set[Form] = Set(HighForm)
  override def feature = Some("Does not contain whens, bulk connects, or uninitialized components")
}
case object LowForm extends Form {
  override def elements: Set[Form] = Set(MidForm)
  override def feature = Some("Does not contain aggregate types")
}
case object WIR extends SingleForm {
  override def feature = Some("Contains some WIR nodes")
}
case object IR extends SingleForm {
  override def feature = Some("Contains some IR versions of WIR nodes")
}
case object Types extends SingleForm {
  override def feature = Some("All types inferred")
}
case object Genders extends SingleForm {
  override def feature = Some("All genders inferred")
}
case object Kinds extends SingleForm {
  override def feature = Some("All kinds inferred")
}
case object Widths extends SingleForm {
  override def feature = Some("All widths inferred")
}
case object Opt extends SingleForm {
  override def feature = Some("Optimized")
}

case object Resolved extends Form {
  val elements = Set(WIR, Not(IR), Types, Genders, Kinds, Widths)
  override def feature = Some("Circuit fully resolved (widths, types, genders, kinds, names)")
}
//case object HighForm extends Form {
//  val elements = Set(WIR, Not(IR), Types, Genders, Kinds, Widths, High, Not(Chirrtl))
//}
