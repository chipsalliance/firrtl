// SPDX-License-Identifier: Apache-2.0

package firrtlTests

import firrtl.testutils._

class ExtModuleTests extends FirrtlFlatSpec {
  "extmodule" should "serialize and re-parse equivalently" in {
    val input =
      """circuit Top :
        |  extmodule Top :
        |    input y : UInt<0>
        |    output x : UInt<1>
        |
        |    defname = ParameterizedExtModule
        |    parameter VALUE = 1
        |    parameter VALUE2 = '2\'d2'
        |    parameter STRING = "one"
        |    parameter REAL = -1.7
        |    parameter TYP = 'bit'
        |    """.stripMargin
    val parsed = parse(input)
    (parse(parsed.serialize)) should be(parsed)
  }
}
