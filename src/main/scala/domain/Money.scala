package mtmc.domain

case class Money(toInt: Int) {
  def +(rhs: Money): Money =
    Money(toInt + rhs.toInt)
}
case object Money {
  implicit val moneyOrdering: Ordering[Money] =
    Ordering.by(_.toInt)
}

