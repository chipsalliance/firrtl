package firrtl.fuzzer

import firrtl.ir._

import scala.annotation.tailrec


trait GenMonad[G[_]] {
  def flatMap[A, B](a: G[A])(f: A => G[B]): G[B]
  def map[A, B](a: G[A])(f: A => B): G[B]

  def flatten[A](gga: G[G[A]]): G[A] = flatMap(gga)(ga => ga)

  def choose(min: Int, max: Int): G[Int]
  def oneOf[A](items: A*): G[A]
  def const[A](c: A): G[A]
  def widen[A, B >: A](ga: G[A]): G[B]

  def bool: G[Boolean] =
    oneOf(true, false)

  def identifier(maxLength: Int): G[String]

  def frequency[a](pairs: (Int, a)*): G[a] = {
    assert(pairs.forall(_._1 > 0))
    assert(pairs.size >= 1)
    val total = pairs.map(_._1).sum
    map(choose(1, total)) { startnum =>
      var idx = 0
      var num = startnum - pairs(idx)._1
      while (num > 0) {
        idx += 1
        num -= pairs(idx)._1
      }
      pairs(idx)._2
    }
  }

  def applyGen[A](ga: G[A]): A
}

/* Functions that take an input state and return a random generator that
 * generates a new state paired with a value
 */
case class StateGen[S, G[_], A](fn: S => G[(S, A)]) {
  final type State[T] = StateGen[S, G, T]

  def flatMap[B](ffn: A => State[B])(implicit GM: GenMonad[G]): State[B] = StateGen { s =>
    GM.flatMap(fn(s)) { case (sx, a) =>
      ffn(a).fn(sx)
    }
  }

  def map[B](f: A => B)(implicit GM: GenMonad[G]): State[B] = StateGen { s =>
    GM.map(fn(s)) { case (sx, a) =>
      sx -> f(a)
    }
  }

  def widen[B >: A](implicit GM: GenMonad[G]): State[B] = StateGen { s =>
    GM.map(fn(s)) { case (s, a) => s -> a }
  }

  def inspect[S, G[_]: GenMonad, B](fn: S => B)(implicit GM: GenMonad[G]): StateGen[S, G, B] = StateGen { s =>
    GM.const(s -> fn(s))
  }

  def modify[S, G[_]: GenMonad, B](fn: S => S)(implicit GM: GenMonad[G]): StateGen[S, G, Unit] = StateGen { s =>
    GM.const(fn(s) -> Unit)
  }
}

object StateGen {
  def lift[S, G[_]: GenMonad, A](a: A): StateGen[S, G, A] = {
    StateGen((s: S) => GenMonad[G].const(s -> a))
  }
  def liftG[S, G[_]: GenMonad, A](ga: G[A]): StateGen[S, G, A] = {
    StateGen((s: S) => GenMonad[G].map(ga)(s -> _))
  }
  def pure[S, G[_]: GenMonad, A](a: A): StateGen[S, G, A] = {
    StateGen((s: S) => GenMonad[G].const(s -> a))
  }

  def inspect[S, G[_]: GenMonad, A](fn: S => A): StateGen[S, G, A] = {
    StateGen(s => GenMonad[G].const((s, fn(s))))
  }
  def inspectG[S, G[_]: GenMonad, A](fn: S => G[A]): StateGen[S, G, A] = {
    StateGen(s => GenMonad[G].map(fn(s)) { s -> _ })
  }
}

trait ExprState[State] {
  def withRef[G[_]: GenMonad](ref: Reference): StateGen[State, G, Reference]
  def exprGen[G[_]: GenMonad](tpe: Type): StateGen[State, G, Expression]
  def unboundRefs(s: State): Set[Reference]
}

object ExprState {
  def apply[S: ExprState] = implicitly[ExprState[S]]
}

object GenMonad {
  def apply[G[_]: GenMonad] = implicitly[GenMonad[G]]

  object syntax {
    final class GenMonadOps[G[_], A](ga: G[A]) {
      def flatMap[B](f: A => G[B])(implicit GM: GenMonad[G]): G[B] = {
        GM.flatMap(ga)(f)
      }
      def map[B](f: A => B)(implicit GM: GenMonad[G]): G[B] = {
        GM.map(ga)(f)
      }
      def widen[B >: A](implicit GM: GenMonad[G]): G[B] = {
        GM.widen[A, B](ga)
      }
    }
    final class GenMonadFlattenOps[G[_], A](gga: G[G[A]]) {
      def flatten(implicit GM: GenMonad[G]): G[A] = GM.flatten(gga)
    }

    implicit final def genMonadOps[G[_], A](ga: G[A]): GenMonadOps[G, A] =
      new GenMonadOps(ga)

    implicit final def genMonadFlattenOps[G[_], A](gga: G[G[A]]): GenMonadFlattenOps[G, A] =
      new GenMonadFlattenOps(gga)
  }
}
