// See LICENSE for license details.

package firrtl.passes
import firrtl.annotations.{CircuitTarget, IsMember}
import firrtl.{CircuitState, RenameMap, Utils}
import firrtl.options.Dependency
import firrtl.stage.TransformManager
import firrtl.stage.TransformManager.TransformDependency
import org.scalatest.flatspec.AnyFlatSpec

class LegacyLowerTypesSpec extends LowerTypesEndToEndSpec {
  override protected def pass: TransformDependency = Dependency(LowerTypes)
}

class NewLowerTypesEndToEndSpec extends LowerTypesEndToEndSpec {
  override protected def pass: TransformDependency = Dependency(NewLowerTypes)
}

class NewLowerTypesUnitTestSpec extends LowerTypesBaseSpec {
  import LowerTypesSpecUtils._
  override protected def lower(n: String, tpe: String, namespace: Set[String]): Seq[String] =
    destruct(n, tpe, namespace).fields
}

/** Runs the lowering pass in the context of the compiler instead of directly calling internal functions. */
abstract class LowerTypesEndToEndSpec extends LowerTypesBaseSpec {
  protected def pass: TransformDependency
  private lazy val lowerTypesCompiler = new TransformManager(Seq(pass))
  private def legacyLower(n: String, tpe: String, namespace: Set[String]): Seq[String] = {
    val inputs = namespace.map(n => s"    input $n : UInt<1>").mkString("\n")
    val src =
      s"""circuit c:
         |  module c:
         |$inputs
         |    output $n : $tpe
         |    $n is invalid
         |""".stripMargin
    val c = CircuitState(firrtl.Parser.parse(src), Seq())
    val c2 =  lowerTypesCompiler.execute(c)
    val ps = c2.circuit.modules.head.ports.filterNot(p => namespace.contains(p.name))
    ps.map{p =>
      val orientation = Utils.to_flip(p.direction)
      s"${orientation.serialize}${p.name} : ${p.tpe.serialize}"}
  }

  override protected def lower(n: String, tpe: String, namespace: Set[String]): Seq[String] =
    legacyLower(n, tpe, namespace)
}

/** this spec can be tested with either the new or the old LowerTypes pass */
abstract class LowerTypesBaseSpec extends AnyFlatSpec {
  protected def lower(n: String, tpe: String, namespace: Set[String] = Set()): Seq[String]

  it should "lower bundles and vectors" in {
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}") == Seq("a_a : UInt<1>", "a_b : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}}") == Seq("a_a : UInt<1>", "a_b_c : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}") == Seq("a_a : UInt<1>", "a_b_0 : UInt<1>", "a_b_1 : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]") ==
      Seq("a_0_a : UInt<1>", "a_0_b : UInt<1>", "a_1_a : UInt<1>", "a_1_b : UInt<1>"))

    // with conflicts
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}", Set("a_a")) == Seq("a__a : UInt<1>", "a__b : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}", Set("a_b")) == Seq("a__a : UInt<1>", "a__b : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}", Set("a_c")) == Seq("a_a : UInt<1>", "a_b : UInt<1>"))

    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}}", Set("a_a")) == Seq("a__a : UInt<1>", "a__b_c : UInt<1>"))
    // in this case we do not have a "real" conflict, but it could be in a reference and thus a is still changed to a_
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}}", Set("a_b")) == Seq("a__a : UInt<1>", "a__b_c : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}}", Set("a_b_c")) == Seq("a__a : UInt<1>", "a__b_c : UInt<1>"))

    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}", Set("a_a")) ==
      Seq("a__a : UInt<1>", "a__b_0 : UInt<1>", "a__b_1 : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}", Set("a_a", "a_b_0")) ==
      Seq("a__a : UInt<1>", "a__b_0 : UInt<1>", "a__b_1 : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}", Set("a_b_0")) ==
      Seq("a__a : UInt<1>", "a__b_0 : UInt<1>", "a__b_1 : UInt<1>"))

    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_0")) ==
      Seq("a__0_a : UInt<1>", "a__0_b : UInt<1>", "a__1_a : UInt<1>", "a__1_b : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_3")) ==
      Seq("a_0_a : UInt<1>", "a_0_b : UInt<1>", "a_1_a : UInt<1>", "a_1_b : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_0_a")) ==
      Seq("a__0_a : UInt<1>", "a__0_b : UInt<1>", "a__1_a : UInt<1>", "a__1_b : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_0_c")) ==
      Seq("a_0_a : UInt<1>", "a_0_b : UInt<1>", "a_1_a : UInt<1>", "a_1_b : UInt<1>"))

    // collisions inside the bundle
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}, b_c : UInt<1>}") ==
      Seq("a_a : UInt<1>", "a_b__c : UInt<1>", "a_b_c : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}, b_b : UInt<1>}") ==
      Seq("a_a : UInt<1>", "a_b_c : UInt<1>", "a_b_b : UInt<1>"))

    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2], b_0 : UInt<1>}") ==
      Seq("a_a : UInt<1>", "a_b__0 : UInt<1>", "a_b__1 : UInt<1>", "a_b_0 : UInt<1>"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2], b_c : UInt<1>}") ==
      Seq("a_a : UInt<1>", "a_b_0 : UInt<1>", "a_b_1 : UInt<1>", "a_b_c : UInt<1>"))
  }

  it should "correctly lower the orientation" in {
    assert(lower("a", "{ flip a : UInt<1>, b : UInt<1>}") == Seq("flip a_a : UInt<1>", "a_b : UInt<1>"))
    assert(lower("a", "{ flip a : UInt<1>[2], b : UInt<1>}") ==
      Seq("flip a_a_0 : UInt<1>", "flip a_a_1 : UInt<1>", "a_b : UInt<1>"))
    assert(lower("a", "{ a : { flip c : UInt<1>, d : UInt<1>}[2], b : UInt<1>}") ==
      Seq("flip a_a_0_c : UInt<1>", "a_a_0_d : UInt<1>", "flip a_a_1_c : UInt<1>", "a_a_1_d : UInt<1>", "a_b : UInt<1>")
    )
  }
}

/** Test the renaming for "regular" references, i.e. Wires, Nodes and Register.
  * Memories and Instances are special cases.
  */
class LowerTypesRenamingSpec extends AnyFlatSpec {
  import LowerTypesSpecUtils._
  protected def lower(n: String, tpe: String, namespace: Set[String] = Set()): RenameMap =
    destruct(n, tpe, namespace).renameMap

  private val m = CircuitTarget("m").module("m")

  it should "not rename ground types" in {
    val r = lower("a", "UInt<1>")
    assert(r.underlying.isEmpty)
  }

  it should "properly rename lowered bundles and vectors" in {
    val a = m.ref("a")

    def one(namespace: Set[String], prefix: String): Unit = {
      val r = lower("a", "{ a : UInt<1>, b : UInt<1>}", namespace)
      assert(get(r,a) == Set(m.ref(prefix + "a"), m.ref(prefix + "b")))
      assert(get(r,a.field("a")) == Set(m.ref(prefix + "a")))
      assert(get(r,a.field("b")) == Set(m.ref(prefix + "b")))
    }
    one(Set(), "a_")
    one(Set("a_a"), "a__")

    def two(namespace: Set[String], prefix: String): Unit = {
      val r =  lower("a", "{ a : UInt<1>, b : { c : UInt<1>}}", namespace)
      assert(get(r,a) == Set(m.ref(prefix + "a"), m.ref(prefix + "b_c")))
      assert(get(r,a.field("a")) == Set(m.ref(prefix + "a")))
      assert(get(r,a.field("b")) == Set(m.ref(prefix + "b_c")))
      assert(get(r,a.field("b").field("c")) == Set(m.ref(prefix + "b_c")))
    }
    two(Set(), "a_")
    two(Set("a_a"), "a__")

    def three(namespace: Set[String], prefix: String): Unit = {
      val r = lower("a", "{ a : UInt<1>, b : UInt<1>[2]}", namespace)
      assert(get(r,a) == Set(m.ref(prefix + "a"), m.ref(prefix + "b_0"), m.ref(prefix + "b_1")))
      assert(get(r,a.field("a")) == Set(m.ref(prefix + "a")))
      assert(get(r,a.field("b")) == Set( m.ref(prefix + "b_0"), m.ref(prefix + "b_1")))
      assert(get(r,a.field("b").index(0)) == Set(m.ref(prefix + "b_0")))
      assert(get(r,a.field("b").index(1)) == Set(m.ref(prefix + "b_1")))
    }
    three(Set(), "a_")
    three(Set("a_b_0"), "a__")

    def four(namespace: Set[String], prefix: String): Unit = {
      val r = lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", namespace)
      assert(get(r,a) == Set(m.ref(prefix + "0_a"), m.ref(prefix + "1_a"), m.ref(prefix + "0_b"), m.ref(prefix + "1_b")))
      assert(get(r,a.index(0)) == Set(m.ref(prefix + "0_a"), m.ref(prefix + "0_b")))
      assert(get(r,a.index(1)) == Set(m.ref(prefix + "1_a"), m.ref(prefix + "1_b")))
      assert(get(r,a.index(0).field("a")) == Set(m.ref(prefix + "0_a")))
      assert(get(r,a.index(0).field("b")) == Set(m.ref(prefix + "0_b")))
      assert(get(r,a.index(1).field("a")) == Set(m.ref(prefix + "1_a")))
      assert(get(r,a.index(1).field("b")) == Set(m.ref(prefix + "1_b")))
    }
    four(Set(), "a_")
    four(Set("a_0"), "a__")
    four(Set("a_3"), "a_")

    // collisions inside the bundle
    {
      val r = lower("a", "{ a : UInt<1>, b : { c : UInt<1>}, b_c : UInt<1>}")
      assert(get(r,a) == Set(m.ref("a_a"), m.ref("a_b__c"), m.ref("a_b_c")))
      assert(get(r,a.field("a")) == Set(m.ref("a_a")))
      assert(get(r,a.field("b")) == Set(m.ref("a_b__c")))
      assert(get(r,a.field("b").field("c")) == Set(m.ref("a_b__c")))
      assert(get(r,a.field("b_c")) == Set(m.ref("a_b_c")))
    }
    {
      val r = lower("a", "{ a : UInt<1>, b : { c : UInt<1>}, b_b : UInt<1>}")
      assert(get(r,a) == Set(m.ref("a_a"), m.ref("a_b_c"), m.ref("a_b_b")))
      assert(get(r,a.field("a")) == Set(m.ref("a_a")))
      assert(get(r,a.field("b")) == Set(m.ref("a_b_c")))
      assert(get(r,a.field("b").field("c")) == Set(m.ref("a_b_c")))
      assert(get(r,a.field("b_b")) == Set(m.ref("a_b_b")))
    }
    {
      val r = lower("a", "{ a : UInt<1>, b : UInt<1>[2], b_0 : UInt<1>}")
      assert(get(r,a) == Set(m.ref("a_a"), m.ref("a_b__0"), m.ref("a_b__1"), m.ref("a_b_0")))
      assert(get(r,a.field("a")) == Set(m.ref("a_a")))
      assert(get(r,a.field("b")) == Set(m.ref("a_b__0"), m.ref("a_b__1")))
      assert(get(r,a.field("b").index(0)) == Set(m.ref("a_b__0")))
      assert(get(r,a.field("b").index(1)) == Set(m.ref("a_b__1")))
      assert(get(r,a.field("b_0")) == Set(m.ref("a_b_0")))
    }
  }
}

/** Instances are a special case since they do not get completely destructed but instead become a 1-deep bundle. */
class LowerInstancesSpec extends AnyFlatSpec {
  import LowerTypesSpecUtils._
  private case class Lower(inst: firrtl.ir.DefInstance, fields: Seq[String], renameMap: RenameMap)
  private val m = CircuitTarget("m").module("m")
  def resultToFieldSeq(res: Seq[(firrtl.ir.SubField, String)]): Seq[String] =
    res.map(_._1).map(r => s"${r.name} : ${r.tpe.serialize}")
  private def lower(n: String, tpe: String, module: String, namespace: Set[String], renames: RenameMap = RenameMap()):
  Lower = {
    val ref = firrtl.ir.DefInstance(firrtl.ir.NoInfo, n, module, parseType(tpe))
    val mutableSet = scala.collection.mutable.HashSet[String]() ++ namespace
    val (newInstance, res) = DestructTypes.destructInstance(m, ref, mutableSet, renames)
    Lower(newInstance, resultToFieldSeq(res), renames)
  }
  private def get(l: Lower, m: IsMember): Set[IsMember] = l.renameMap.get(m).get.toSet

  it should "not rename instances if the instance name does not change" in {
    val l = lower("i", "{ a : UInt<1>}", "c", Set())
    assert(l.renameMap.underlying.isEmpty)
  }

  it should "lower an instance correctly" in {
    val i = m.instOf("i", "c")
    val l = lower("i", "{ a : UInt<1>}", "c", Set("i_a"))
    assert(l.inst.name == "i_")
    assert(l.inst.tpe.isInstanceOf[firrtl.ir.BundleType])
    assert(l.inst.tpe.serialize == "{ a : UInt<1>}")

    assert(get(l, i) == Set(m.instOf("i_", "c")))
    assert(l.fields == Seq("a : UInt<1>"))
  }

  it should "update the rename map with the changed port names" in {
    // without lowering ports
    {
      val i = m.instOf("i", "c")
      val l = lower("i", "{ b : { c : UInt<1>}, b_c : UInt<1>}", "c", Set("i_b_c"))
      // the instance was renamed because of the collision with "i_b_c"
      assert(get(l, i) == Set(m.instOf("i_", "c")))
      // the rename of e.g. `instance.b` to `instance_.b__c` was not recorded since we never performed the
      // port renaming and thus we won't get a result
      assert(get(l, i.ref("b")) == Set(m.instOf("i_", "c").ref("b")))
    }

    // same as above but with lowered port
    {
      // We need two distinct rename maps: one for the port renaming and one for everything else.
      // This is to accommodate the use-case where a port as well as an instance needs to be renames
      // thus requiring a two-stage translation process for reference to the port of the instance.
      // This two-stage translation is only supported through chaining rename maps.
      val portRenames = RenameMap()
      val otherRenames = RenameMap()

      // The child module "c" which we assume has the following ports: b : { c : UInt<1>} and b_c : UInt<1>
      val c = CircuitTarget("m").module("c")
      val portB = firrtl.ir.Port(firrtl.ir.NoInfo, "b", firrtl.ir.Output, parseType("{ c : UInt<1>}"))
      val portB_C = firrtl.ir.Port(firrtl.ir.NoInfo, "b_c", firrtl.ir.Output, parseType("UInt<1>"))

      // lower ports
      val namespaceC = scala.collection.mutable.HashSet[String]() ++ Seq("b", "b_c")
      DestructTypes.destruct(c, portB, namespaceC, portRenames)
      DestructTypes.destruct(c, portB_C, namespaceC, portRenames)
      // only port b is renamed, port b_c stays the same
      assert(portRenames.get(c.ref("b")).get == Seq(c.ref("b__c")))

      // in module m we then lower the instance i of c
      val l = lower("i", "{ b : { c : UInt<1>}, b_c : UInt<1>}", "c", Set("i_b_c"), otherRenames)
      val i = m.instOf("i", "c")
      // the instance was renamed because of the collision with "i_b_c"
      val i_ = m.instOf("i_", "c")
      assert(get(l, i) == Set(i_))

      // the ports renaming is also noted
      val r = portRenames.andThen(otherRenames)
      assert(r.get(i.ref("b")).get == Seq(i_.ref("b__c")))
      assert(r.get(i.ref("b").field("c")).get == Seq(i_.ref("b__c")))
      assert(r.get(i.ref("b_c")).get == Seq(i_.ref("b_c")))
     }
  }
}

/** Memories are a special case as they remain 2-deep bundles and fields of the datatype are pulled into the front.
  * E.g., `mem.r.data.a` becomes `mem_a.r.data`
  */
class LowerMemorySpec extends AnyFlatSpec {
  import LowerTypesSpecUtils._
  private case class Lower(mems: Seq[firrtl.ir.DefMemory], refs: Seq[(firrtl.ir.SubField, String)],
                           renameMap: RenameMap)
  private val m = CircuitTarget("m").module("m")
  private val mem = m.ref("mem")
  private def lower(name: String, tpe: String, namespace: Set[String],
                      r: Seq[String] = List("r"), w: Seq[String] = List("w")): Lower = {
    val dataType = parseType(tpe)
    val mem = firrtl.ir.DefMemory(firrtl.ir.NoInfo, name, dataType, depth = 2, writeLatency = 1, readLatency = 1,
      readUnderWrite = firrtl.ir.ReadUnderWrite.Undefined, readers = r, writers = w, readwriters = Seq())
    val renames = RenameMap()
    val mutableSet = scala.collection.mutable.HashSet[String]() ++ namespace
    val(mems, refs) = DestructTypes.destructMemory(m, mem, mutableSet, renames)
    Lower(mems, refs, renames)
  }
  private val UInt1 = firrtl.ir.UIntType(firrtl.ir.IntWidth(1))

  it should "not rename anything for a ground type memory if there was no conflict" in {
    val l = lower("mem", "UInt<1>", Set("mem_r", "mem_r_data"), w=Seq("w"))
    assert(l.renameMap.underlying.isEmpty)
  }

  it should "rename even ground type memories if there are conflicts on the ports" in {
    // Conflicting port names are not a problem for lower types, since ports do not get flattened at this stage.
    // However, they later do get flattened and it is easiest to just do the renaming here.
    val r = lower("mem", "UInt<1>", Set("mem_r", "mem_r_data"), w=Seq("r_data")).renameMap

    assert(r.get(mem).isEmpty, "memory was not renamed")
    assert(r.get(mem.field("r")).get == Seq(mem.field("r_")), "read port was renamed")
    assert(r.underlying.size == 1, "only change should be the port rename")
  }

  it should "rename references to lowered and/or renamed ports" in {
    val r = lower("mem", "{ a : UInt<1>, b : UInt<1>}", Set("mem_a"), r=Seq("r", "r_data")).renameMap

    // complete memory
    assert(get(r, mem) == Set(m.ref("mem__a"), m.ref("mem__b")))

    // read ports
    assert(get(r, mem.field("r")) ==
      Set(m.ref("mem__a").field("r_"), m.ref("mem__b").field("r_")))
    assert(get(r, mem.field("r_data")) ==
      Set(m.ref("mem__a").field("r_data"), m.ref("mem__b").field("r_data")))

    // port fields
    assert(get(r, mem.field("r").field("data")) ==
      Set(m.ref("mem__a").field("r_").field("data"),
        m.ref("mem__b").field("r_").field("data")))
    assert(get(r, mem.field("r").field("addr")) ==
      Set(m.ref("mem__a").field("r_").field("addr"),
        m.ref("mem__b").field("r_").field("addr")))
    assert(get(r, mem.field("r").field("en")) ==
      Set(m.ref("mem__a").field("r_").field("en"),
        m.ref("mem__b").field("r_").field("en")))
    assert(get(r, mem.field("r").field("clk")) ==
      Set(m.ref("mem__a").field("r_").field("clk"),
        m.ref("mem__b").field("r_").field("clk")))
    assert(get(r, mem.field("w").field("mask")) ==
      Set(m.ref("mem__a").field("w").field("mask"),
        m.ref("mem__b").field("w").field("mask")))

    // port sub-fields
    assert(get(r, mem.field("r").field("data").field("a")) ==
      Set(m.ref("mem__a").field("r_").field("data")))
    assert(get(r, mem.field("r").field("data").field("b")) ==
      Set(m.ref("mem__b").field("r_").field("data")))

    // need to rename the following:
    // mem -> mem__a, mem__b
    // mem.r -> mem__a.r_, mem__b.r_
    // mem.r.data.{a,b} -> mem__{a,b}.r_.data
    // mem.w.data.{a,b} -> mem__{a,b}.w.data
    // mem.r_data.data.{a,b} -> mem__{a,b}.r_data.data
    val renameCount = r.underlying.map(_._2.size).sum
    assert(renameCount == 10, "it is enough to rename *to* 10 different signals")
    assert(r.underlying.size == 8, "it is enough to rename (from) 8 different signals")
  }

  it should "rename references for a memory with a nested data type" in {
    val l = lower("mem", "{ a : UInt<1>, b : { c : UInt<1>} }", Set("mem_a"))
    assert(l.mems.map(_.name) == Seq("mem__a", "mem__b_c"))
    assert(l.mems.map(_.dataType) == Seq(UInt1, UInt1))

    // complete memory
    val r = l.renameMap
    assert(get(r, mem) == Set(m.ref("mem__a"), m.ref("mem__b_c")))

    // read port
    assert(get(r, mem.field("r")) ==
      Set(m.ref("mem__a").field("r"), m.ref("mem__b_c").field("r")))

    // port sub-fields
    assert(get(r, mem.field("r").field("data").field("a")) ==
      Set(m.ref("mem__a").field("r").field("data")))
    assert(get(r, mem.field("r").field("data").field("b")) ==
      Set(m.ref("mem__b_c").field("r").field("data")))
    assert(get(r, mem.field("r").field("data").field("b").field("c")) ==
      Set(m.ref("mem__b_c").field("r").field("data")))

    val renameCount = r.underlying.map(_._2.size).sum
    assert(renameCount == 8, "it is enough to rename *to* 8 different signals")
    assert(r.underlying.size == 7, "it is enough to rename (from) 7 different signals")
  }

  it should "rename references for vector type memories" in {
    val l = lower("mem", "UInt<1>[2]", Set("mem_0"))
    assert(l.mems.map(_.name) == Seq("mem__0", "mem__1"))
    assert(l.mems.map(_.dataType) == Seq(UInt1, UInt1))

    // complete memory
    val r = l.renameMap
    assert(get(r, mem) == Set(m.ref("mem__0"), m.ref("mem__1")))

    // read port
    assert(get(r, mem.field("r")) ==
      Set(m.ref("mem__0").field("r"), m.ref("mem__1").field("r")))

    // port sub-fields
    assert(get(r, mem.field("r").field("data").index(0)) ==
      Set(m.ref("mem__0").field("r").field("data")))
    assert(get(r, mem.field("r").field("data").index(1)) ==
      Set(m.ref("mem__1").field("r").field("data")))

    val renameCount = r.underlying.map(_._2.size).sum
    assert(renameCount == 6, "it is enough to rename *to* 6 different signals")
    assert(r.underlying.size == 5, "it is enough to rename (from) 5 different signals")
  }

}

private object LowerTypesSpecUtils {
  private val typedCompiler = new TransformManager(Seq(Dependency(InferTypes)))
  def parseType(tpe: String): firrtl.ir.Type = {
    val src =
      s"""circuit c:
         |  module c:
         |    input c: $tpe
         |""".stripMargin
    val c = CircuitState(firrtl.Parser.parse(src), Seq())
    typedCompiler.execute(c).circuit.modules.head.ports.head.tpe
  }
  case class DestructResult(fields: Seq[String], renameMap: RenameMap)
  def destruct(n: String, tpe: String, namespace: Set[String]): DestructResult = {
    val ref = firrtl.ir.Field(n, firrtl.ir.Default, parseType(tpe))
    val renames = RenameMap()
    val mutableSet = scala.collection.mutable.HashSet[String]() ++ namespace
    val res = DestructTypes.destruct(m, ref, mutableSet, renames)
    DestructResult(resultToFieldSeq(res), renames)
  }
  def resultToFieldSeq(res: Seq[(firrtl.ir.Field, String)]): Seq[String] =
    res.map(_._1).map(r => s"${r.flip.serialize}${r.name} : ${r.tpe.serialize}")
  def get(r: RenameMap, m: IsMember): Set[IsMember] = r.get(m).get.toSet
  protected val m = CircuitTarget("m").module("m")
}