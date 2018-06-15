import scala.annotation.tailrec

object GameScorer {

  def scoreFrames(frames: List[Frame]): List[(Frame, Int)] = {

    require(frames.nonEmpty)

    @tailrec
    def recurse(remainingFrames: List[Frame], scoreTracker: List[(Frame, Int)]): List[(Frame, Int)] = {
      remainingFrames match {
        case currentFrame :: remaining =>
          val proceedingFrames = scoreTracker.map(_._1)
          val frameScore = scoreForFrame(proceedingFrames, currentFrame)
          recurse(remaining, (currentFrame, frameScore) +: scoreTracker)
        case Nil => scoreTracker
      }
    }

    val reversed = frames.reverse
    val scoredFrames = recurse(reversed, Nil)
    accumulateScores(scoredFrames)
  }

  private def accumulateScores(scoredFrames: List[(Frame, Int)]) =
    scoredFrames.zipWithIndex.map {
      case ((frame, score), index) => (frame, score + scoredFrames.take(index).map(_._2).sum)
    }

  private def scoreForFrame(nextFrames: List[Frame], currentFrame: Frame) = {`
    val nextTwoFrames = nextFrames.take(2)
    nextTwoFrames match {
      case List(next, nextNext) =>
        val nextNextThrow = next match {
          case NormalFrame(_, _, Strike) => Some(nextNext.throwOne)
          case NormalFrame(_, t2Opt, _) => t2Opt
          case _ => throw new RuntimeException("Final Frame encountered in wrong position")
        }
        FrameScorer.scoreFrame(currentFrame, Some(next.throwOne), nextNextThrow)
      case List(last: FinalFrame) =>
        FrameScorer.scoreFrame(currentFrame, Some(last.throwOne), last.throwTwo)
      case Nil =>
        FrameScorer.scoreFrame(currentFrame, None, None)
    }
  }
}


