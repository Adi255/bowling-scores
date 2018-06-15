
sealed trait Frame {
  val throwOne: Int
  val throwTwo: Option[Int]
  def baseScore: Int
}

case class NormalFrame(throwOne: Int, throwTwo: Option[Int], bonusType: BonusType) extends Frame {
  override def baseScore: Int = throwOne + throwTwo.getOrElse(0)
}

case class FinalFrame(throwOne: Int, throwTwo: Option[Int], throwThree: Option[Int]) extends Frame {
  override def baseScore: Int = throwOne + throwTwo.getOrElse(0) + throwThree.getOrElse(0)
}

sealed trait BonusType

case object NoBonus extends BonusType

case object Strike extends BonusType

case object Spare extends BonusType