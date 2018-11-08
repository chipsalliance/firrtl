package firrtl.analyses.component

trait SubComponent
case class Instance(name: String, module: String) extends SubComponent { override def toString = s"($name of $module)" }
case class Name(name: String) extends SubComponent { override def toString = name }
case class Index(value: Int) extends SubComponent { override def toString = s"[$value]" }
case class Field(value: String) extends SubComponent { override def toString = s".$value" }
case class Arg(index: Int) extends SubComponent { override def toString = s"@arg@$index" }
case class Anonymous(value: String) extends SubComponent { override def toString = value }
case class Bit(index: Int) extends SubComponent { override def toString = s"@bit@$index" }
case object Clock extends SubComponent { override def toString = "@clock@" }
case object Init extends SubComponent { override def toString = "@init@" }
case object Reset extends SubComponent { override def toString = "@reset@" }

case class Component(encapsulatingModule: String, reference: Seq[SubComponent], tag: Int) {
  def field(name: String): Component = this.copy(reference = reference :+ Field(name))
  def index(value: Int): Component = this.copy(reference = reference :+ Index(value))
  def bit(value: Int): Component = this.copy(reference = reference :+ Bit(value))
  def arg(index: Int): Component = {
    assert(reference.last.isInstanceOf[Anonymous])
    this.copy(reference = reference :+ Arg(index))
  }
  def clock: Component = this.copy(reference = reference :+ Clock)
  def init: Component = this.copy(reference = reference :+ Init)
  def reset: Component = this.copy(reference = reference :+ Reset)
  override def toString(): String = s"$$$tag$$$encapsulatingModule/${reference.map(_.toString).mkString("")}"
}

private object Component {
  private[component] val counter = new java.util.concurrent.atomic.AtomicInteger(0)
}

object Referable {
  def apply(name: String, encapsulatingModule: String): Component = {
    Component(encapsulatingModule, Seq(Name(name)), (Component.counter.incrementAndGet()))
  }
}

object Irreferable {
  def apply(value: String, encapsulatingModule: String): Component = {
    Component(encapsulatingModule, Seq(Anonymous(value)), (Component.counter.incrementAndGet()))
  }
}



