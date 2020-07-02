// See LICENSE for license details.

package firrtl.ir

/** Helper functions and classes for keeping the number of FirrtlNodes created small.
  *
  * The most important thing to note is: The firrtl compiler does not do **strict** interning,
  * i.e., only because two FirrtlNodes are not `eq`, the could still contain the same data.
  * However, in an effort to reduce the number of object used in the compiler we try to intern
  * often used expressions and types.
  *
  * Our strategy is three fold:
  * 1. We provide interning expression and type factories for use in the protobuf and firrtl parsers.
  * 2. We modify the `mapExpr` and `mapType` methods to only copy of the returned object is not `eq` to the prior value.
  * 3. We provide an object identity based cache so that, e.g., passes that work on expressions can replace
  *    all expression instances 1:1 instead of creating a new node at every place that the expression is used.
  */
object Interning {

}
