
object FrameScorer {

  def scoreFrame(frame: Frame, nextThrow: Option[Int] = None, nextNextThrow: Option[Int] = None): Int = {
    frame match {
      case NormalFrame(_, _, NoBonus) => frame.baseScore
      case NormalFrame(_, _, Strike) => nextThrow.getOrElse(0) + nextNextThrow.getOrElse(0) + frame.baseScore
      case NormalFrame(_, _, Spare) => nextThrow.getOrElse(0) + frame.baseScore
      case f@FinalFrame(_, _, _) => f.baseScore
    }
  }
}
