// See LICENSE for license details.

package firrtl.passes
import firrtl.annotations.{CircuitTarget, IsMember}
import firrtl.{CircuitState, RenameMap, Utils}
import firrtl.options.Dependency
import firrtl.stage.TransformManager
import org.scalatest.flatspec.AnyFlatSpec

class LowerTypesSpec extends LowerTypesBaseSpec {
  private val lowerTypesCompiler = new TransformManager(Seq(Dependency(LowerTypes)))
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

class NewLowerTypesSpec extends LowerTypesBaseSpec {
  import LowerTypesSpecUtils._
  override protected def lower(n: String, tpe: String, namespace: Set[String]): Seq[String] =
    destruct(n, tpe, namespace).fields
}


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

// Test the renaming for "regular" references, i.e. Wires, Nodes and Register. Memories and Instances are special cases.
class LowerTypesRenamingSpec extends AnyFlatSpec {
  import LowerTypesSpecUtils._
  protected def lower(n: String, tpe: String, namespace: Set[String] = Set()): RenameMap =
    destruct(n, tpe, namespace).renameMap

  it should "properly rename lowered bundles and vectors" in {
    def get(r: RenameMap, m: IsMember): Set[IsMember] = r.get(m).get.toSet
    val m = CircuitTarget("c").module("c")
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

class LowerInstancesSpec extends AnyFlatSpec {
  import LowerTypesSpecUtils._
  protected def lower(n: String, tpe: String, namespace: Set[String]): Unit = {
    val ref = firrtl.ir.Field(n, firrtl.ir.Default, parseType(tpe))
    val renames = RenameMap()
    val mutableSet = scala.collection.mutable.HashSet[String]() ++ namespace
    val parent = CircuitTarget("c").module("c")
    val (newInstance, refs) = DestructTypes.destructInstance(parent, ref, mutableSet, renames)
    println()
  }

  // Instances are a special case since they do not get completely destructed but instead become a 1-deep bundle.


  it should "lower an instance correctly" in {
    lower("instance", "{ a : UInt<1>}", Set("instance_a"))
  }
}

class LowerMemorySpec extends AnyFlatSpec {
  import LowerTypesSpecUtils._
  protected def lower(name: String, tpe: String, namespace: Set[String],
                      r: Seq[String] = List("r"), w: Seq[String] = List("w")): Unit = {
    val dataType = parseType(tpe)
    val mem = firrtl.ir.DefMemory(firrtl.ir.NoInfo, name, dataType, depth = 2, writeLatency = 1, readLatency = 1,
      readUnderWrite = firrtl.ir.ReadUnderWrite.Undefined, readers = r, writers = w, readwriters = Seq())
    val renames = RenameMap()
    val mutableSet = scala.collection.mutable.HashSet[String]() ++ namespace
    val parent = CircuitTarget("c").module("c")
    DestructTypes.destructMemory(parent, mem, mutableSet, renames)
    println()
  }

  // Memories are a special case.


  it should "lower an memory correctly" in {
    lower("mem", "{ a : UInt<1>}", Set("mem_a"))
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
    val parent = CircuitTarget("c").module("c")
    val fieldsAndRefs = DestructTypes.destruct(parent, ref, mutableSet, renames)
    val fields = fieldsAndRefs.map(_._1).map(r => s"${r.flip.serialize}${r.name} : ${r.tpe.serialize}")
    DestructResult(fields, renames)
  }
}