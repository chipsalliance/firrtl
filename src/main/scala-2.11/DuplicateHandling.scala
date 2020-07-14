package firrtl.options

import scopt.OptionParser

import firrtl.AnnotationSeq

/** A modified OptionParser with mutable termination and additional checks
  */
trait DuplicateHandling extends OptionParser[AnnotationSeq] {

  override def parse(args: Seq[String], init: AnnotationSeq): Option[AnnotationSeq] = {

    /** Message for found duplicate options */
    def msg(x: String, y: String) = s"""Duplicate $x "$y" (did your custom Transform or OptionsManager add this?)"""

    val longDups = options.map(_.name).groupBy(identity).collect{ case (k, v) if v.size > 1 && k != "" => k }
    val shortDups = options.map(_.shortOpt).flatten.groupBy(identity).collect{ case (k, v) if v.size > 1 => k }


    if (longDups.nonEmpty)  {
      throw new OptionsException(msg("long option", longDups.map("--" + _).mkString(",")), new IllegalArgumentException)
    }

    if (shortDups.nonEmpty) {
      throw new OptionsException(msg("short option", shortDups.map("-" + _).mkString(",")), new IllegalArgumentException)
    }

    super.parse(args, init)
  }

}