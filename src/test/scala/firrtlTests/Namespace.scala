
package firrtlTests

import firrtl._

import java.io._

class NameExpansionSpec extends FirrtlPropSpec {

  property("Names should expand properly (even with collisions)") {
    assert(runFirrtlTest("NameExpansion", "/features"))
  }
}
