/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/

package firrtl.interpreter

import firrtl._

import collection.mutable

/**
  * This is the evaluation engine for the FirrtlTerp
  * it requires the previousState of the system
  *
  * @param circuitState  the state of the system, should not be modified before all dependencies have been resolved
  */
class LoFirrtlExpressionEvaluator(
                                   startKeys: Iterable[String],
                                   dependencyGraph: DependencyGraph,
                                   circuitState: CircuitState) {
  val toResolve = mutable.HashSet(startKeys.toSeq:_*)
  val inProcess = toResolve.empty

  private var resolveDepth = 0
  private var verbose = false
  def setVerbose(value: Boolean): Unit = {verbose = value }
  private def indent(): Unit = resolveDepth += 1
  private def dedent(): Unit = resolveDepth -= 1
  private def log(message: => String): Unit = {
    if(verbose) {
      println(s"${" "*(resolveDepth*2)}$message")
    }
  }

  /**
    * get the value from the current circuit state, if it is dependent on something else
    * we haven't computed yet. resolve this new dependency first then pull it's value from the
    * current state
    *
    * @param key  the name of the assignable thing
    * @return
    */
  def getValue(key: String): ConcreteValue = {
    circuitState.getValue(key).getOrElse(resolveDependency(key))
  }

  /**
    * mask off bits above size in a BigInt,
    * uses modulo, constructing the modulo base on the size
    * working around BigInt's shift by int requirement
    *
    * @param number number to mask
    * @param size   how many bits to keep
    * @return
    */
  def mask(number: BigInt, size: BigInt): BigInt = {
    val convenientShiftSize = 30
    var modulo: BigInt = 1
    var toShift: BigInt = (size - 1).max(0) + 1
    while(toShift > 0) {
      modulo = modulo << toShift.min(convenientShiftSize).toInt
      toShift -= convenientShiftSize
    }
    number % modulo
  }

  /**
    * shifts number right
    *
    * @param number number to shift
    * @param size how many bits to shift
    * @return
    */
  def shiftRight(number: BigInt, size: BigInt): BigInt = {
    val convenientShiftSize = 30
    var toShift: BigInt = size.max(0)
    var shiftedNumber: BigInt = number
    while(toShift > 0) {
      shiftedNumber = shiftedNumber >> toShift.min(convenientShiftSize).toInt
      toShift -= convenientShiftSize
    }
    shiftedNumber
  }
  /**
    * shifts number left
    *
    * @param number number to shift
    * @param size how many bits to shift
    * @return
    */
  def shiftLeft(number: BigInt, size: BigInt): BigInt = {
    val convenientShiftSize = 30
    var toShift: BigInt = size.max(0)
    var shiftedNumber: BigInt = number
    while(toShift > 0) {
      shiftedNumber = shiftedNumber << toShift.min(convenientShiftSize).toInt
      toShift -= convenientShiftSize
    }
    shiftedNumber
  }

  def makeUIntValue(value: BigInt, intWidth: IntWidth): UIntValue = {
    val maskedValue = mask(value, intWidth.width)
    UIntValue(maskedValue, intWidth)
  }

  def makeSIntValue(value: BigInt, intWidth: IntWidth): SIntValue = {
    val maskedValue = mask(value, intWidth.width)
    SIntValue(maskedValue, intWidth)
  }

  def getWidth(tpe: Type): IntWidth = {
    val intWidth: IntWidth = tpe match {
      case UIntType(width: IntWidth) => width
      case SIntType(width: IntWidth) => width
    }
    intWidth
  }

  def mathPrimitive(opCode: PrimOp, args: Seq[Expression], tpe: Type): ConcreteValue = {
    val arg1 = evaluate(args.head)
    val arg2 = evaluate(args.tail.head)
    opCode match {
      case ADD_OP => (arg1, arg2) match {
        case (e1: UIntValue, e2: UIntValue) => makeUIntValue(e1.value + e2.value, getWidth(tpe))
        case (e1: UIntValue, e2: SIntValue) => makeSIntValue(e1.value + e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: UIntValue) => makeSIntValue(e1.value + e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: SIntValue) => makeSIntValue(e1.value + e2.value, getWidth(tpe))
      }
      case SUB_OP => (arg1, arg2) match {
        case (e1: UIntValue, e2: UIntValue) => makeSIntValue(e1.value - e2.value, getWidth(tpe))
        case (e1: UIntValue, e2: SIntValue) => makeSIntValue(e1.value - e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: UIntValue) => makeSIntValue(e1.value - e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: SIntValue) => makeSIntValue(e1.value - e2.value, getWidth(tpe))
      }
      case MUL_OP => (arg1, arg2) match {
        case (e1: UIntValue, e2: UIntValue) => makeUIntValue(e1.value * e2.value, getWidth(tpe))
        case (e1: UIntValue, e2: SIntValue) => makeSIntValue(e1.value * e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: UIntValue) => makeSIntValue(e1.value * e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: SIntValue) => makeSIntValue(e1.value * e2.value, getWidth(tpe))
      }
      case DIV_OP => (arg1, arg2) match {
        case (e1: UIntValue, e2: UIntValue) => makeUIntValue(e1.value / e2.value, getWidth(tpe))
        case (e1: UIntValue, e2: SIntValue) => makeUIntValue(e1.value / e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: UIntValue) => makeSIntValue(e1.value / e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: SIntValue) => makeSIntValue(e1.value / e2.value, getWidth(tpe))
      }
      case REM_OP => (arg1, arg2) match {
        case (e1: UIntValue, e2: UIntValue) => makeUIntValue(e1.value % e2.value, getWidth(tpe))
        case (e1: UIntValue, e2: SIntValue) => makeSIntValue(e1.value % e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: UIntValue) => makeSIntValue(e1.value % e2.value, getWidth(tpe))
        case (e1: SIntValue, e2: SIntValue) => makeSIntValue(e1.value % e2.value, getWidth(tpe))
      }
    }
  }

  def comparisonOp(opCode: PrimOp, args: Seq[Expression], tpe: Type): ConcreteValue = {
    val arg1 = evaluate(args.head)
    val arg2 = evaluate(args.tail.head)
    opCode match {
      case EQUAL_OP      => makeUIntValue(if(arg1.value == arg2.value) 1 else 0, getWidth(tpe))
      case NEQUAL_OP     => makeUIntValue(if(arg1.value != arg2.value) 1 else 0, getWidth(tpe))
      case LESS_OP       => makeUIntValue(if(arg1.value <  arg2.value) 1 else 0, getWidth(tpe))
      case LESS_EQ_OP    => makeUIntValue(if(arg1.value <= arg2.value) 1 else 0, getWidth(tpe))
      case GREATER_OP    => makeUIntValue(if(arg1.value >  arg2.value) 1 else 0, getWidth(tpe))
      case GREATER_EQ_OP => makeUIntValue(if(arg1.value >= arg2.value) 1 else 0, getWidth(tpe))
    }
  }

  def paddingOp(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): ConcreteValue = {
    val arg = evaluate(args.head)

    opCode match {
      case padOp => arg match {
        // padding is handled by the width being used in concrete value instantiation
        case u: UIntValue => makeUIntValue(u.value, getWidth(tpe))
        case u: SIntValue => makeSIntValue(u.value, getWidth(tpe))
      }
    }
  }

  def castingOp(opCode: PrimOp, args: Seq[Expression], tpe: Type): ConcreteValue = {
    val arg = evaluate(args.head)

    opCode match {
      case AS_UINT_OP => arg match {
        // padding is handled by the width being used in concrete value instantiation
        case u: UIntValue  => makeUIntValue(u.value, getWidth(tpe))
        case u: SIntValue  => makeUIntValue(u.value, getWidth(tpe))
        case u: ClockValue => makeUIntValue(u.value, getWidth(tpe))
      }
      case AS_SINT_OP => arg match {
        // padding is handled by the width being used in concrete value instantiation
        case u: UIntValue  => makeSIntValue(u.value, getWidth(tpe))
        case u: SIntValue  => makeSIntValue(u.value, getWidth(tpe))
        case u: ClockValue => makeSIntValue(u.value, getWidth(tpe))
      }
      case AS_CLOCK_OP => arg match {
        // padding is handled by the width being used in concrete value instantiation
        case u: UIntValue  => makeSIntValue(u.value, getWidth(tpe))
        case u: SIntValue  => makeSIntValue(u.value, getWidth(tpe))
        case u: ClockValue => makeSIntValue(u.value, getWidth(tpe))
      }
    }
  }

  def bitOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): ConcreteValue = {
    val e = evaluate(args.head)
    val n = parameters.head
    val argWidth = e.widthAsBigInt

    opCode match {
      case SHIFT_LEFT_OP =>
        assert(n > 0, s"SHIFT_LEFT_OP(${args.head}, $n): parameter must be greater than zero")

        e match {
          case UIntValue(value, _) => makeUIntValue(shiftLeft(value, n), getWidth(tpe))
          case SIntValue(value, _) => makeSIntValue(shiftLeft(value, n), getWidth(tpe))
        }
      case SHIFT_RIGHT_OP =>
        assert(n > 0, s"SHIFT_RIGHT_OP(${args.head}, $n): parameter must be greater than zero")
        assert(argWidth >= n,
          s"SHIFT_RIGHT_OP(${args.head}, $n): parameter must be less than or equal to width args")

        e match {
          case UIntValue(value, _) => makeUIntValue(shiftRight(value, n), getWidth(tpe))
          case SIntValue(value, _) => makeSIntValue(shiftRight(value, n), getWidth(tpe))
        }
      case HEAD_OP =>
        assert(argWidth >= n,
          s"HEAD_OP(${args.head}, $n): parameter must be less than or equal to width args")
        assert(n > 0, s"tail_op(${args.head}, $n): parameter must be greater than zero")

        val shiftSize = argWidth - n
        makeUIntValue(shiftRight(e.value, shiftSize), getWidth(tpe))
      case TAIL_OP =>
        assert(argWidth > n,
          s"TAIL_OP(${args.head}, $n): parameter $n must be strictly less than width args")
        assert(n > 0, s"TAIL_OP(${args.head}, $n): parameter $n must be greater than zero")

        val maskSize = (argWidth - n).max(0)
        makeUIntValue(mask(e.value, maskSize), getWidth(tpe))
    }
  }
  def dynamicBitOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): ConcreteValue = {
    val e = evaluate(args.head)
    val n = evaluate(args.tail.head)

    opCode match {
      case DYN_SHIFT_LEFT_OP =>
        assert(n.value > 0, s"DYN_SHIFT_LEFT(${args.head}, $n): parameter must be greater than zero")
        assert(n.isInstanceOf[UIntValue], "DYN_SHIFT_LEFT($args), n must be UIntValue")

        e match {
          case UIntValue(value, _) => makeUIntValue(shiftLeft(value, n.value), getWidth(tpe))
          case SIntValue(value, _) => makeSIntValue(shiftLeft(value, n.value), getWidth(tpe))
        }
      case DYN_SHIFT_RIGHT_OP =>
        assert(n.value > 0, s"tail_op(${args.head}, $n): parameter must be greater than zero")
        assert(n.isInstanceOf[UIntValue], "DYN_SHIFT_RIGHT($args), n must be UIntValue")
        assert(e.widthAsBigInt >= n.value,
          s"tail_op(${args.head}, $n): parameter must be less than or equal to width args")

        e match {
          case UIntValue(value, _) => makeUIntValue(shiftRight(value, n.value), getWidth(tpe))
          case SIntValue(value, _) => makeSIntValue(shiftRight(value, n.value), getWidth(tpe))
        }
    }
  }
  /**
    * evaluate expression, if this expression references an ephemeral value (wire or node) that has
    * not been evaluated yet, recursively evaluate that reference first.  LoFirrtl guarantees that
    * there will be no loops here
    *
    * @param expression a LoFirrtl expression to evaluate
    * @return the resulting ConcreteValue
    *
    * Note: OpCodes here are double matched, once in main loop herein, then again in function suitable for that
    * family of opCodes, it makes the code cleaner, I think, but may ultimately need to be inlined for performance
    */
  def evaluate(expression: Expression): ConcreteValue = {
    log(s"evaluate $expression")
    indent()

    val result = expression match {
      case mux: Mux =>
        if( evaluate(mux.cond).value > 0 ) {
          evaluate(mux.tval)
        }
        else {
          evaluate(mux.fval)
        }
      case WRef(name, tpe, kind, gender) => getValue(name)
      case DoPrim(op, args, const, tpe) =>
        op match {
          case ADD_OP             => mathPrimitive(op, args, tpe)
          case SUB_OP             => mathPrimitive(op, args, tpe)
          case MUL_OP             => mathPrimitive(op, args, tpe)
          case DIV_OP             => mathPrimitive(op, args, tpe)
          case REM_OP             => mathPrimitive(op, args, tpe)

          case EQUAL_OP           => comparisonOp(op, args, tpe)
          case NEQUAL_OP          => comparisonOp(op, args, tpe)
          case LESS_OP            => comparisonOp(op, args, tpe)
          case LESS_EQ_OP         => comparisonOp(op, args, tpe)
          case GREATER_OP         => comparisonOp(op, args, tpe)
          case GREATER_EQ_OP      => comparisonOp(op, args, tpe)

          case PAD_OP             => paddingOp(op, args, const, tpe)

          case AS_UINT_OP         => castingOp(op, args, tpe)
          case AS_SINT_OP         => castingOp(op, args, tpe)
          case AS_CLOCK_OP        => castingOp(op, args, tpe)

          case DYN_SHIFT_LEFT_OP  => dynamicBitOps(op, args, const, tpe)
          case DYN_SHIFT_RIGHT_OP => dynamicBitOps(op, args, const, tpe)

          case SHIFT_LEFT_OP      => bitOps(op, args, const, tpe)
          case SHIFT_RIGHT_OP     => bitOps(op, args, const, tpe)
          case HEAD_OP            => bitOps(op, args, const, tpe)
          case TAIL_OP            => bitOps(op, args, const, tpe)

          case _ =>
            throw new InterruptedException(s"PrimOP $op in $expression not yet supported")
        }
      case c: ConcreteValue => c
    }

    dedent()
    log(s"evaluator:returns:$result")

    result
  }

  private def resolveDependency(key: String): ConcreteValue = {
    assert(toResolve.contains(key))
    toResolve -= key

    log(s"resolveDependency:start: $key")
    resolveDepth += 1

    val value = if(circuitState.isInput(key)) {
      circuitState.getValue(key).get
    }
    else {
      val expression = dependencyGraph.nameToExpression(key)
      evaluate(expression)
    }
    circuitState.setValue(key, value)

    resolveDepth -= 1
    log(s"resolveDependency:done: $key <= $value")

    value
  }

  def resolveDependencies(): Unit = {
    while (toResolve.nonEmpty) {
      val key = toResolve.head
      resolveDependency(key)
    }
  }

  def checkStops(): Option[Int] = {
    for(stopStatement <- dependencyGraph.stops) {
      if(evaluate(stopStatement.en).value > 0) {
        if(stopStatement.ret == 0) {
          println(s"Success:${stopStatement.info}")
          return Some(0)
        }
        else {
          println(s"Failure:${stopStatement.info} returned ${stopStatement.ret}")
          return Some(stopStatement.ret)
        }
      }
    }
    None
  }

  def checkPrints(): Unit = {
    for(printStatment <- dependencyGraph.prints) {
      val condition = evaluate(printStatment.en)
      if(condition.value > 0) {
        val resolvedArgs = printStatment.args.map { case arg =>
          evaluate(arg).value
        }
        val formatString = printStatment.string.array.map(_.toChar).mkString("")
        printf(formatString, resolvedArgs:_*)
      }
    }
  }
}
