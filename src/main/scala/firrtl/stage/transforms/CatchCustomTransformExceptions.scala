// See LICENSE for license details.

package firrtl.stage.transforms

import firrtl.{CircuitState, CustomTransformException, Transform}
import firrtl.stage.CircuitPhase

class CatchCustomTransformExceptions(val underlying: CircuitPhase) extends CircuitPhase with WrappedTransform {

  override def transform(c: CircuitState): CircuitState = try {
    underlying.transform(c)
  } catch {
    case e: Exception if CatchCustomTransformExceptions.isCustomTransform(trueUnderlying) => throw CustomTransformException(e)
  }

}

object CatchCustomTransformExceptions {

  private[firrtl] def isCustomTransform(xform: CircuitPhase): Boolean = {
    def getTopPackage(pack: java.lang.Package): java.lang.Package =
      Package.getPackage(pack.getName.split('.').head)
    // We use the top package of the Driver to get the top firrtl package
    Option(xform.getClass.getPackage).map { p =>
      getTopPackage(p) != firrtl.Driver.getClass.getPackage
    }.getOrElse(true)
  }

  def apply(a: CircuitPhase): CatchCustomTransformExceptions = new CatchCustomTransformExceptions(a)

}
