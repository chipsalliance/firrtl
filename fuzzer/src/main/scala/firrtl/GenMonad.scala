package firrtl.fuzzer

/** Monads that represent a random value generator
  */
trait GenMonad[Gen[_]] {
  def flatMap[A, B](a: Gen[A])(f: A => Gen[B]): Gen[B]
  def map[A, B](a: Gen[A])(f: A => B): Gen[B]

  def flatten[A](gga: Gen[Gen[A]]): Gen[A] = flatMap(gga)(ga => ga)

  def choose(min: Int, max: Int): Gen[Int]
  def oneOf[A](items: A*): Gen[A]
  def const[A](c: A): Gen[A]
  def widen[A, B >: A](ga: Gen[A]): Gen[B]

  def applyGen[A](ga: Gen[A]): A
}

object GenMonad {
  def apply[Gen[_]: GenMonad] = implicitly[GenMonad[Gen]]

  def bool[Gen[_]: GenMonad]: Gen[Boolean] = GenMonad[Gen].oneOf(true, false)

  /** Creates a generator that generates values based on the weights paired with each value
    */
  def frequency[Gen[_]: GenMonad, A](pairs: (Int, A)*): Gen[A] = {
    assert(pairs.forall(_._1 > 0))
    assert(pairs.size >= 1)
    val total = pairs.map(_._1).sum
    GenMonad[Gen].map(GenMonad[Gen].choose(1, total)) { startnum =>
      var idx = 0
      var num = startnum - pairs(idx)._1
      while (num > 0) {
        idx += 1
        num -= pairs(idx)._1
      }
      pairs(idx)._2
    }
  }

  /** Provides extension methods like .flatMap and .flatten for [[GenMonad]]s
    */
  object syntax {
    final class GenMonadOps[Gen[_], A](ga: Gen[A]) {
      def flatMap[B](f: A => Gen[B])(implicit GM: GenMonad[Gen]): Gen[B] = {
        GM.flatMap(ga)(f)
      }
      def map[B](f: A => B)(implicit GM: GenMonad[Gen]): Gen[B] = {
        GM.map(ga)(f)
      }
      def widen[B >: A](implicit GM: GenMonad[Gen]): Gen[B] = {
        GM.widen[A, B](ga)
      }
    }
    final class GenMonadFlattenOps[Gen[_], A](gga: Gen[Gen[A]]) {
      def flatten(implicit GM: GenMonad[Gen]): Gen[A] = GM.flatten(gga)
    }

    implicit def genMonadOps[Gen[_], A](ga: Gen[A]): GenMonadOps[Gen, A] =
      new GenMonadOps(ga)

    implicit def genMonadFlattenOps[Gen[_], A](gga: Gen[Gen[A]]): GenMonadFlattenOps[Gen, A] =
      new GenMonadFlattenOps(gga)
  }
}
