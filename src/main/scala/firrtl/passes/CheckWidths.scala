// See LICENSE for license details.

package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.PrimOps._
import firrtl.traversals.Foreachers._
import firrtl.Utils._
import firrtl.annotations.{CircuitTarget, ModuleTarget, Target, TargetToken}

object CheckWidths extends Pass {
  /** The maximum allowed width for any circuit element */
  val MaxWidth = 1000000
  val DshlMaxWidth = ceilLog2(MaxWidth + 1)
  class UninferredWidth (info: Info, target: String) extends PassException(
    s"""|$info : Uninferred width for target below. (Did you forget to assign to it?)
        |$target""".stripMargin)
  class WidthTooSmall(info: Info, mname: String, b: BigInt) extends PassException(
    s"$info : [target $mname]  Width too small for constant $b.")
  class WidthTooBig(info: Info, mname: String, b: BigInt) extends PassException(
    s"$info : [target $mname]  Width $b greater than max allowed width of $MaxWidth bits")
  class DshlTooBig(info: Info, mname: String) extends PassException(
    s"$info : [target $mname]  Width of dshl shift amount cannot be larger than $DshlMaxWidth bits.")
  class NegWidthException(info:Info, mname: String) extends PassException(
    s"$info: [target $mname] Width cannot be negative or zero.")
  class BitsWidthException(info: Info, mname: String, hi: BigInt, width: BigInt, exp: String) extends PassException(
    s"$info: [target $mname] High bit $hi in bits operator is larger than input width $width in $exp.")
  class HeadWidthException(info: Info, mname: String, n: BigInt, width: BigInt) extends PassException(
    s"$info: [target $mname] Parameter $n in head operator is larger than input width $width.")
  class TailWidthException(info: Info, mname: String, n: BigInt, width: BigInt) extends PassException(
    s"$info: [target $mname] Parameter $n in tail operator is larger than input width $width.")
  class AttachWidthsNotEqual(info: Info, mname: String, eName: String, source: String) extends PassException(
    s"$info: [target $mname] Attach source $source and expression $eName must have identical widths.")

  def run(c: Circuit): Circuit = {
    val errors = new Errors()

    def check_width_w(info: Info, target: Target)(w: Width): Unit = {
      w match {
        case IntWidth(width) if width >= MaxWidth =>
          errors.append(new WidthTooBig(info, target.serialize, width))
        case w: IntWidth if w.width >= 0 =>
        case _: IntWidth =>
          errors append new NegWidthException(info, target.serialize)
        case _ =>
          errors append new UninferredWidth(info, target.prettyPrint("    "))
      }
    }

    def hasWidth(tpe: Type): Boolean = tpe match {
      case GroundType(IntWidth(w)) => true
      case GroundType(_) => false
      case _ => throwInternalError(s"hasWidth - $tpe")
    }

    def check_width_t(info: Info, target: Target)(t: Type): Unit = {
      t match {
        case tt: BundleType => tt.fields.foreach(check_width_f(info, target))
        case tt => tt foreach check_width_t(info, target)
      }
      t foreach check_width_w(info, target)
    }

    def check_width_f(info: Info, target: Target)(f: Field): Unit =
      check_width_t(info, target.modify(tokens = target.tokens :+ TargetToken.Field(f.name)))(f.tpe)

    def check_width_e_leaf(info: Info, target: Target, expr: Expression): Unit = {
      // This is a leaf check of the "local" width-correctness of one expression node, so no recursion.
      expr match {
        case e @ UIntLiteral(v, w: IntWidth) if math.max(1, v.bitLength) > w.width =>
          errors.append(new WidthTooSmall(info, target.serialize, v))
        case e @ SIntLiteral(v, w: IntWidth) if v.bitLength + 1 > w.width =>
          errors.append(new WidthTooSmall(info, target.serialize, v))
        case e @ DoPrim(op, Seq(a, b), _, tpe) =>
          (op, a.tpe, b.tpe) match {
            case (Dshl, at, bt) if (hasWidth(at) && bitWidth(bt) >= DshlMaxWidth) =>
              errors.append(new DshlTooBig(info, target.serialize))
            case _ =>
          }
        case e @ DoPrim(op, Seq(a), consts, _) =>
          (op, consts) match {
            case (Bits, Seq(hi, lo)) if (hasWidth(a.tpe) && bitWidth(a.tpe) <= hi) =>
              errors.append(new BitsWidthException(info, target.serialize, hi, bitWidth(a.tpe), e.serialize))
            case (Head, Seq(n)) if (hasWidth(a.tpe) && bitWidth(a.tpe) < n) =>
              errors.append(new HeadWidthException(info, target.serialize, n, bitWidth(a.tpe)))
            case (Tail, Seq(n)) if (hasWidth(a.tpe) && bitWidth(a.tpe) < n) =>
              errors.append(new TailWidthException(info, target.serialize, n, bitWidth(a.tpe)))
            case _ =>
          }
        case _ =>
      }
    }

    def check_width_e(info: Info, target: Target, recDepth: Integer)(expr: Expression): Unit = {
      check_width_e_leaf(info, target, expr)
      expr match {
        case _: Mux | _: ValidIf | _: DoPrim =>
          // Width errors only occur on lit / Mux / DoPrim; only Mux, ValidIf, and DoPrim can have these as children
          if (recDepth > 0)
            // Use recursion for trees up to a nominal depth to avoid overhead
            expr.foreach(check_width_e(info, target, recDepth - 1))
          else
            // Beyond that, switch to an explicit, iterative DFS to avoid stack overflow
            check_width_e_dfs(info, target, expr)
        case _ => // Anything else can only contain references / sub{fields, indices, accesses}
      }
    }

    def check_width_e_dfs(info: Info, target: Target, expr: Expression): Unit = {
      val stack = collection.mutable.ArrayStack(expr)
      def push(e: Expression): Unit = stack.push(e)
      while (stack.nonEmpty) {
        val current = stack.pop()
        check_width_e_leaf(info, target, current)
        current.foreach(push)
      }
    }

    def check_width_s(minfo: Info, target: ModuleTarget)(s: Statement): Unit = {
      val info = get_info(s) match { case NoInfo => minfo case x => x }
      val subRef = s match { case sx: HasName => target.ref(sx.name) case _ => target }
      s foreach check_width_e(info, target, 4)
      s foreach check_width_s(info, target)
      s foreach check_width_t(info, subRef)
      s match {
        case Attach(infox, exprs) =>
          exprs.tail.foreach ( e =>
            if (bitWidth(e.tpe) != bitWidth(exprs.head.tpe))
              errors.append(new AttachWidthsNotEqual(infox, target.serialize, e.serialize, exprs.head.serialize))
          )
        case sx: DefRegister =>
          sx.reset.tpe match {
            case UIntType(IntWidth(w)) if w == 1 =>
            case AsyncResetType =>
            case ResetType =>
            case _ => errors.append(new CheckTypes.IllegalResetType(info, target.serialize, sx.name))
          }
        case _ =>
      }
    }

    def check_width_p(minfo: Info, target: ModuleTarget)(p: Port): Unit = check_width_t(p.info, target)(p.tpe)

    def check_width_m(circuit: CircuitTarget)(m: DefModule): Unit = {
      m foreach check_width_p(m.info, circuit.module(m.name))
      m foreach check_width_s(m.info, circuit.module(m.name))
    }

    c.modules foreach check_width_m(CircuitTarget(c.main))
    errors.trigger()
    c
  }
}
