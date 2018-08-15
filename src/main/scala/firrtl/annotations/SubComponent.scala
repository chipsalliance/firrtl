package firrtl.annotations

trait SubComponent {
  def keyword: String
  def value: Any
  def is(keywords: String*): Boolean = {
    keywords.map { kw =>
      val lastClass = this.getClass
      lastClass == SubComponent.keyword2subcomponent(kw)("0").getClass()
    }.reduce(_ || _)
  }
  //override def toString = s"/$keyword@$value"
}

case object SubComponent {
  //implicit def string2int(s: String): Int = s.toInt
  case class Instance(value: String)  extends SubComponent { override def keyword: String = "inst" }
  case class OfModule(value: String)  extends SubComponent { override def keyword: String = "of" }
  case class Ref(value: String)       extends SubComponent { override def keyword: String = "ref" }
  case class Index(value: Int)        extends SubComponent { override def keyword: String = "[]" }
  case class Field(value: String)     extends SubComponent { override def keyword: String = "." }
  case class Arg(value: Int)          extends SubComponent { override def keyword: String = "arg" }
  case class Anonymous(value: String) extends SubComponent { override def keyword: String = "" }
  case class Bit(value: Int)          extends SubComponent { override def keyword: String = "bit" }
  case object Clock                   extends SubComponent { override def keyword: String = "clock"; val value = "" }
  case object Init                    extends SubComponent { override def keyword: String = "init";  val value = "" }
  case object Reset                   extends SubComponent { override def keyword: String = "reset"; val value = "" }
  case object Regs                    extends SubComponent { override def keyword: String = "regs";  val value = "" }

  val keyword2subcomponent = Map(
    "inst" -> ((value: String) => Instance(value)),
    "of" -> ((value: String) => OfModule(value)),
    "ref" -> ((value: String) => Ref(value)),
    "[]" -> ((value: String) => Index(value.toInt)),
    "." -> ((value: String) => Field(value)),
    "arg" -> ((value: String) => Arg(value.toInt)),
    "" -> ((value: String) => Anonymous(value)),
    "bit" -> ((value: String) => Bit(value.toInt)),
    "clock" -> ((value: String) => Clock),
    "init" -> ((value: String) => Init),
    "reset" -> ((value: String) => Reset),
    "regs" -> ((value: String) => Regs)
  )
}

