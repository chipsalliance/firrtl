// SPDX-License-Identifier: Apache-2.0

package firrtl.ir

import firrtl.{ir => fir}

import scala.collection.mutable

object UnifiedTypes {

  sealed trait Type {
    def passive: Boolean
    protected def passiveString = passive match {
      case true => "(p)"
      case false => ""
    }

    protected def canonicalize: Type

    protected def flip: Type
  }

  case class Flip(wrapped: Type) extends Type {
    val passive = false

    override def toString = s"$$flip<$wrapped>"

    override protected def canonicalize: Type = wrapped match {
      /* flip(flip(a)) -> a */
      case Flip(a) => a
      case _       => Flip(wrapped)
    }

    override protected def flip = wrapped

  }

  object Flip {

    def get(a: Type): Type = Flip(a).canonicalize

  }

  case object Ground extends Type {
    override def passive = true

    override def toString = "$ground"

    override protected def canonicalize = this

    override protected def flip = Flip(this)
  }

  case class Bundle(elements: scala.collection.Map[String, Type]) extends Type {
    override def passive = elements.forall{ case (_, a) => a.passive }

    override def toString = elements.map{ case (a, b) => s"$a: $b" }.mkString("$bundle<",", ",">")

    override protected def canonicalize = elements.forall {
      case (_, _: Flip) => true
      case _            => false
    } match {
      /* All elements are flip, so flip them and wrap in a flip */
      case true  => flip
      case false => this
    }

    override protected def flip = Flip(Bundle(elements.mapValues(Flip.get(_))))
  }

  object Bundle {

    def get(elements: scala.collection.Map[String, Type]): Type = {
      Bundle(elements).canonicalize
    }

  }

  implicit class TypeHelpers(tpe: fir.Type) {

    def asUnified: Type = tpe match {
      case _: fir.GroundType => Ground
      case fir.BundleType(fields) =>
        val fieldsx =
          mutable.LinkedHashMap.empty[String, Type] ++
            fields.map {
              case fir.Field(n, fir.Flip, t) => (n -> Flip.get(t.asUnified))
              case fir.Field(n, _, t)       => (n -> t.asUnified)
            }
        Bundle.get(fieldsx)
      /* Squash aggregates since they cannot flip */
      case a: fir.VectorType => a.tpe.asUnified
    }

  }

}
