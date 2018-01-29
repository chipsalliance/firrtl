package stateTests

import org.scalatest._
import firrtl.form._

class ScheduleTests extends FlatSpec {
  behavior of "Scheduler"

  // Group of possible transforms
//  val transforms: Seq[Transform] = Seq(
//    new WIRTransform(),
//    new TypesTransform(),
//    new GendersTransform(),
//    new KindsTransform(),
//    new WidthsTransform(),
//    new ResolvedConversion(),
//    new HighToMidTransform(),
//    new DoStuff()
//  )
//
//  "Simple schedule" should "order transformations" in {
//    val schedule = Schedule(IR, Middle, transforms)
//    println(schedule.order())
//  }
//  "Simple schedule 2" should "order transformations" in {
//    val schedule = Schedule(IR, CircuitForm(Middle, Stuff, WIR, Resolved), transforms)
//    println(schedule.order())
//  }

}
