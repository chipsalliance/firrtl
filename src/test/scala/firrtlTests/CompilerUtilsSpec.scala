// SPDX-License-Identifier: Apache-2.0

package firrtlTests

import firrtl._
import firrtl.testutils._

class CompilerUtilsSpec extends FirrtlFlatSpec {

  def genTransform(_inputForm: CircuitForm, _outputForm: CircuitForm) = new Transform {
    def inputForm = _inputForm
    def outputForm = _outputForm
    def execute(state: CircuitState): CircuitState = state
  }

  // Core lowering transforms
  val chirrtlToHigh = genTransform(ChirrtlForm, HighForm)
  val highToMid = genTransform(HighForm, MidForm)
  val midToLow = genTransform(MidForm, LowForm)
  val chirrtlToLowList = List(chirrtlToHigh, highToMid, midToLow)

  // Custom transforms
  val chirrtlToChirrtl = genTransform(ChirrtlForm, ChirrtlForm)
  val highToHigh = genTransform(HighForm, HighForm)
  val midToMid = genTransform(MidForm, MidForm)
  val lowToLow = genTransform(LowForm, LowForm)

  val lowToHigh = genTransform(LowForm, HighForm)

  val lowToLowTwo = genTransform(LowForm, LowForm)
}
