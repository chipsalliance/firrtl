// SPDX-License-Identifier: Apache-2.0

package firrtl.stage.transforms

import firrtl.{CircuitState, Transform}

class ReportSizeChange(val underlying: Transform) extends Transform with WrappedTransform {

  override def execute(c: CircuitState): CircuitState = {
    lazy val cx = underlying.transform(c)

    logger.info{
      val oldSize = c.circuit.toString.size
      val newSize = cx.circuit.toString.size
      val percentDiff = f"${(newSize - oldSize) / oldSize.toDouble}%.2f"
      f"""|Circuit Size Information (characters):
          |  Old Size: $oldSize
          |  New Size: $newSize
          |  Change: $percentDiff%%""".stripMargin
    }

    cx
  }

}
