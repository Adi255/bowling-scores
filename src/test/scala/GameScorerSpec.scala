import org.scalatest.{FlatSpec, Matchers}

class GameScorerSpec extends FlatSpec with Matchers {

  behavior of "Game Scorer"

  it can "calculate scores for regular frames" in {

    val input = List.fill(9)(NormalFrame(3, Some(3), NoBonus)) :+ FinalFrame(3, Some(3), None)

    val scores = GameScorer.scoreFrames(input)

    scores.reverse.head._2 shouldBe 60
  }

  it can "calculate scores for normal frames with correct order" in {

    val input = (1 to 9).map(i => NormalFrame(i, None, NoBonus)).toList :+ FinalFrame(10, Some(10), Some(10))

    val scores = GameScorer.scoreFrames(input)
    val expected = List(
      (NormalFrame(1, None, NoBonus), 1),
      (NormalFrame(2, None, NoBonus), 3),
      (NormalFrame(3, None, NoBonus), 6),
      (NormalFrame(4, None, NoBonus), 10),
      (NormalFrame(5, None, NoBonus), 15),
      (NormalFrame(6, None, NoBonus), 21),
      (NormalFrame(7, None, NoBonus), 28),
      (NormalFrame(8, None, NoBonus), 36),
      (NormalFrame(9, None, NoBonus), 45),
      (FinalFrame(10, Some(10), Some(10)), 75)
    )
    scores should contain theSameElementsInOrderAs expected
  }


  it can "calculate scores for some bonus frames" in {

    val input = List(
      NormalFrame(9, Some(1), Spare), //13
      NormalFrame(3, Some(4), NoBonus), //7
      NormalFrame(10, None, Strike), //18
      FinalFrame(5, Some(3), None) //8
    )

    val scores = GameScorer.scoreFrames(input)

    val expected = List(
      (NormalFrame(9, Some(1), Spare), 13),
      (NormalFrame(3, Some(4), NoBonus), 20),
      (NormalFrame(10, None, Strike), 38),
      (FinalFrame(5, Some(3), None), 46)
    )

    scores should contain theSameElementsInOrderAs expected
  }

  it can "calculate scores for programming praxis test case" in {

    val input = List(
      NormalFrame(1, Some(4), NoBonus),
      NormalFrame(4, Some(5), NoBonus),
      NormalFrame(6, Some(4), Spare),
      NormalFrame(5, Some(5), Spare),
      NormalFrame(10, None, Strike),
      NormalFrame(0, Some(1), NoBonus),
      NormalFrame(7, Some(3), Spare),
      NormalFrame(6, Some(4), Spare),
      NormalFrame(10, None, Strike),
      FinalFrame(2, Some(8), Some(6))
    )

    val scores = GameScorer.scoreFrames(input)

    val expected = List(
      (NormalFrame(1, Some(4), NoBonus), 5),
      (NormalFrame(4, Some(5), NoBonus), 14),
      (NormalFrame(6, Some(4), Spare), 29),
      (NormalFrame(5, Some(5), Spare), 49),
      (NormalFrame(10, None, Strike), 60),
      (NormalFrame(0, Some(1), NoBonus), 61),
      (NormalFrame(7, Some(3), Spare), 77),
      (NormalFrame(6, Some(4), Spare), 97),
      (NormalFrame(10, None, Strike), 117),
      (FinalFrame(2, Some(8), Some(6)), 133)
    )

    scores should contain theSameElementsInOrderAs expected
  }

  it can "get correct scores for perfect game" in {
    val input = List(
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      NormalFrame(10, None, Strike),
      FinalFrame(10, Some(10), Some(10))
    )

    val scores = GameScorer.scoreFrames(input)

    val expected = List(
      (NormalFrame(10, None, Strike), 30),
      (NormalFrame(10, None, Strike), 60),
      (NormalFrame(10, None, Strike), 90),
      (NormalFrame(10, None, Strike), 120),
      (NormalFrame(10, None, Strike), 150),
      (NormalFrame(10, None, Strike), 180),
      (NormalFrame(10, None, Strike), 210),
      (NormalFrame(10, None, Strike), 240),
      (NormalFrame(10, None, Strike), 270),
      (FinalFrame(10, Some(10), Some(10)), 300)
    )

    scores should contain theSameElementsInOrderAs expected
  }

}
