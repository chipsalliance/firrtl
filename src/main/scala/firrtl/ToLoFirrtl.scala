package firrtl

import firrtl.passes._

/**
  * Created by chick on 4/21/16.
  */
object ToLoFirrtl {
  // TODO: Verify this is the proper subset of passes that guarantees LoFirrtl
  val passes = Seq(
    CInferTypes,
    CInferMDir,
    RemoveCHIRRTL,
    ToWorkingIR,
    CheckHighForm,
    ResolveKinds,
    InferTypes,
    CheckTypes,
    Uniquify,
    ResolveKinds,
    InferTypes,
    ResolveGenders,
    CheckGenders,
    InferWidths,
    CheckWidths,
    PullMuxes,
    ExpandConnects,
    RemoveAccesses,
    ExpandWhens,
    CheckInitialization,
    Legalize,
    ResolveKinds,
    InferTypes,
    ResolveGenders,
    InferWidths,
    LowerTypes,
    ResolveKinds,
    InferTypes,
    ResolveGenders,
    InferWidths,
    ConstProp
  )

  def lower(c: Circuit): Circuit = {
    PassUtils.executePasses(c, passes)
  }
}
