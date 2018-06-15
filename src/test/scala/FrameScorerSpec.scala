import org.scalatest.{FlatSpec, Matchers}

class FrameScorerSpec extends FlatSpec with Matchers {

  behavior of "FrameScorer"

  it should "calculate score for normal frame (no spare or strike)" in {
    val frame = NormalFrame(1, Some(2), NoBonus)
    FrameScorer.scoreFrame(frame) shouldBe 3
  }

  it should "calculate score for normal spare frame" in {
    val frame = NormalFrame(5, Some(5), Spare)
    FrameScorer.scoreFrame(frame, Some(4)) shouldBe 14
  }

  it should "calculate score when missing next throw with spare" in {
    val frame = NormalFrame(7, Some(3), Spare)
    FrameScorer.scoreFrame(frame, Some(0), Some(2)) shouldBe 10
  }

  it should "calculate score for normal strike frame" in {
    val frame = NormalFrame(10, None, Strike)
    FrameScorer.scoreFrame(frame, Some(9), Some(1)) shouldBe 20
  }

  it should "calculate score when missing next throw in strike frame" in {
    val frame = NormalFrame(10, None, Strike)
    FrameScorer.scoreFrame(frame, Some(0), Some(8)) shouldBe 18
  }

  it should "calculate score for regular final frame" in {
    val frame = FinalFrame(3, Some(5), None)
    FrameScorer.scoreFrame(frame) shouldBe 8
  }

  it should "calculate score for final frame with spare" in {
    val frame = FinalFrame(3, Some(7), Some(4))
    FrameScorer.scoreFrame(frame) shouldBe 14
  }

  it should "calculate score for final frame with one strike" in {
    val frame = FinalFrame(10, Some(3), Some(4))
    FrameScorer.scoreFrame(frame) shouldBe 17
  }

  it should "calculate score for final frame with two strikes" in {
    val frame = FinalFrame(10, Some(10), Some(5))
    FrameScorer.scoreFrame(frame) shouldBe 25
  }

  it should "calculate score for final frame with three strikes" in {
    val frame = FinalFrame(10, Some(10), Some(10))
    FrameScorer.scoreFrame(frame) shouldBe 30
  }

  it should "calculate score for final frame with spare then strike" in {
    val frame = FinalFrame(0, Some(10), Some(10))
    FrameScorer.scoreFrame(frame) shouldBe 20
  }


}
