// See LICENSE for license details.

package firrtlTests
package transforms

import firrtl.RenameMap
import firrtl.annotations._
import firrtl.transforms.LoadMemoryBundle


/**
 * Tests inline instances transformation
 */
class LoadMemoryTests extends HighTransformSpec {
  def transform = new LoadMemoryBundle

  "A LoadMemoryAnnotation for MemBase[T <: Bits]" should "be preserved" in {
     val input =
        """circuit Top :
          |  module Top :
          |    cmem testMem : UInt<32>[42]
          """.stripMargin
     val check =
        """circuit Top :
          |  module Top :
          |    mem testMem :
          |     data-type => UInt<32>
          |     depth => 42
          |     read-latency => 0
          |     write-latency => 1
          |     read-under-write => undefined
          """.stripMargin
     val anno = loadMemoryFromFile("testMem", "memfile")
     val annos = execute(input, check, Seq(anno)).annotations
     annos.toSeq should contain (anno)
  }

  "A LoadMemoryAnnotation for MemBase[T <: Bundle]" should "become annotations for bundle fields" in {
     val input =
        """circuit Top :
          |  module Top :
          |    cmem testMem : { field1: UInt<9>, inner: { field2: UInt<12> }}[42]
          """.stripMargin
     val check =
        """circuit Top :
          |  module Top :
          |    mem testMem :
          |     data-type => { field1: UInt<9>, inner: { field2: UInt<12> }}
          |     depth => 42
          |     read-latency => 0
          |     write-latency => 1
          |     read-under-write => undefined
          """.stripMargin
     val anno = loadMemoryFromFile("testMem", "memfile")
     val annos = execute(input, check, Seq(anno)).annotations.toSeq
     annos should not contain (anno)
     annos should contain (loadMemoryFromFile("testMem_field1", "memfile_field1"))
     annos should contain (loadMemoryFromFile("testMem_inner_field2", "memfile_inner_field2"))
  }
}
