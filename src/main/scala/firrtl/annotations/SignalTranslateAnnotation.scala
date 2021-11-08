package firrtl.annotations

case class SignalTranslateAnnotation(target: ReferenceTarget, definition: Seq[(BigInt, String)], width: Int)
    extends SingleTargetAnnotation[ReferenceTarget] {
  override def duplicate(n: ReferenceTarget): Annotation = this.copy(n)
}
