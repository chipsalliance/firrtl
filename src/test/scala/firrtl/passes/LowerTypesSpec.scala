// See LICENSE for license details.

package firrtl.passes
import firrtl.annotations.CircuitTarget
import firrtl.{CircuitState, RenameMap}
import firrtl.options.Dependency
import firrtl.stage.TransformManager
import org.scalatest.flatspec.AnyFlatSpec

class LowerTypesSpec extends LowerTypesBaseSpec {
  val uniquifyCompiler = new TransformManager(Seq(Dependency(Uniquify)))
  val lowerTypesCompiler = new TransformManager(Seq(Dependency(LowerTypes)))
  private def legacyLower(n: String, tpe: String, namespace: Set[String], onlyUniquify: Boolean): Seq[String] = {
    val inputs = namespace.map(n => s"    input $n : UInt<1>").mkString("\n")
    val src =
      s"""circuit c:
         |  module c:
         |$inputs
         |    input $n : $tpe
         |""".stripMargin
    val c = CircuitState(firrtl.Parser.parse(src), Seq())
    val c2 = if(onlyUniquify) { uniquifyCompiler.execute(c) }
    else { lowerTypesCompiler.execute(c) }
    val ps = c2.circuit.modules.head.ports.filterNot(p => namespace.contains(p.name))
    ps.map(p => s"${p.name} : ${p.tpe.serialize}")
  }

  override protected def lower(n: String, tpe: String, namespace: Set[String])
                   (implicit opts: LowerTypesOptions): Seq[String] =
    legacyLower(n, tpe, namespace, opts.onlyUniquify)
}

class NewLowerTypesSpec extends LowerTypesBaseSpec {
  val typedCompiler = new TransformManager(Seq(Dependency(InferTypes)))
  private def parseType(tpe: String): firrtl.ir.Type = {
    val src =
      s"""circuit c:
         |  module c:
         |    input c: $tpe
         |""".stripMargin
    val c = CircuitState(firrtl.Parser.parse(src), Seq())
    typedCompiler.execute(c).circuit.modules.head.ports.head.tpe
  }
  override protected def lower(n: String, tpe: String, namespace: Set[String])
                              (implicit opts: LowerTypesOptions): Seq[String] = {
    val ref = firrtl.ir.Field(n, firrtl.ir.Default, parseType(tpe))
    val renames = RenameMap()
    val mutableSet = scala.collection.mutable.HashSet[String]() ++ namespace
    val parent = CircuitTarget("c").module("c")
    new DestructTypes(opts).destruct(parent, ref, mutableSet, renames).map(r => s"${r.name} : ${r.tpe.serialize}")
  }
}


abstract class LowerTypesBaseSpec extends AnyFlatSpec {
  protected def lower(n: String, tpe: String, namespace: Set[String] = Set())
                   (implicit opts: LowerTypesOptions): Seq[String]

  it should "lower bundles and vectors" in {
    implicit val opts = LowerTypesOptions(lowerBundles = true, lowerVecs = true, onlyUniquify = false)

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

  it should "only uniquify bundles and vectors" ignore {
    implicit val opts = LowerTypesOptions(lowerBundles = true, lowerVecs = true, onlyUniquify = true)

    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}") == Seq("a : { a : UInt<1>, b : UInt<1>}"))
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}}") == Seq("a : { a : UInt<1>, b : { c : UInt<1>}}"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}") == Seq("a : { a : UInt<1>, b : UInt<1>[2]}"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]") == Seq("a : { a : UInt<1>, b : UInt<1>}[2]"))

    // with conflicts
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}", Set("a_a")) == Seq("a_ : { a : UInt<1>, b : UInt<1>}"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}", Set("a_b")) == Seq("a_ : { a : UInt<1>, b : UInt<1>}"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}", Set("a_c")) == Seq("a : { a : UInt<1>, b : UInt<1>}"))

    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}}", Set("a_a")) ==
      Seq("a_ : { a : UInt<1>, b : { c : UInt<1>}}"))
    // in this case we do not have a "real" conflict, but it appears so initially and thus a is still changed to a_
    assert(lower("a", "{ a: UInt<1>, b: { c : UInt<1>}}", Set("a_b")) ==
      Seq("a_ : { a : UInt<1>, b : { c : UInt<1>}}"))
    assert(lower("a", "{ a: UInt<1>, b: { c : UInt<1>}}", Set("a_b_c")) ==
      Seq("a_ : { a : UInt<1>, b : { c : UInt<1>}}"))

    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}", Set("a_a")) ==
      Seq("a_ : { a : UInt<1>, b : UInt<1>[2]}"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}", Set("a_a", "a_b_0")) ==
      Seq("a_ : { a : UInt<1>, b : UInt<1>[2]}"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2]}", Set("a_b_0")) ==
      Seq("a_ : { a : UInt<1>, b : UInt<1>[2]}"))

    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_0")) ==
      Seq("a_ : { a : UInt<1>, b : UInt<1>}[2]"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_3")) ==
      Seq("a : { a : UInt<1>, b : UInt<1>}[2]"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_0_a")) ==
      Seq("a_ : { a : UInt<1>, b : UInt<1>}[2]"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>}[2]", Set("a_0_c")) ==
      Seq("a : { a : UInt<1>, b : UInt<1>}[2]"))

    // collisions inside the bundle
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}, b_c : UInt<1>}") ==
      Seq("a : { a : UInt<1>, b_ : { c : UInt<1>}, b_c : UInt<1>}"))
    assert(lower("a", "{ a : UInt<1>, b : { c : UInt<1>}, b_b : UInt<1>}") ==
      Seq("a : { a : UInt<1>, b : { c : UInt<1>}, b_b : UInt<1>}"))

    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2], b_0 : UInt<1>}") ==
      Seq("a : { a : UInt<1>, b_ : UInt<1>[2], b_0 : UInt<1>}"))
    assert(lower("a", "{ a : UInt<1>, b : UInt<1>[2], b_c : UInt<1>}") ==
      Seq("a : { a : UInt<1>, b : UInt<1>[2], b_c : UInt<1>}"))
  }
}
