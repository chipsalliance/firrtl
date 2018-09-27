package firrtlTests

import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import firrtl.Utils

class UtilsSpec extends FlatSpec {

  behavior of "Utils.expandPrefix"

  val expandPrefixTests = List(
    ("expand an unprefixed name",
     "foo", Set("foo")),
    ("expand a name ending in a prefix",
     "foo__", Set("foo__")),
    ("expand a name with a prefix",
     "foo_bar", Set("foo_bar", "foo_")),
    ("expand a complex name",
     "foo__$ba9_X__$$$$$_", Set("foo__$ba9_X__$$$$$_", "foo__$ba9_X__", "foo__$ba9_", "foo__")),
    ("expand a name starting with a delimiter",
     "__foo_bar", Set("__", "__foo_", "__foo_bar")))

  for ((description, in, out) <- expandPrefixTests) {
    it should description in { Utils.expandPrefixes(in).toSet should be (out)}
  }
}
