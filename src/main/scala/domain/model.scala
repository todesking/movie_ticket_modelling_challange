package mtmc.domain


case class Fee(base: Money, extras: Seq[ExtraFee]) {
  val total: Money = extras.map(_.amount).foldLeft(base)(_ + _)
}

case class ExtraFee(name: String, amount: Money)

// あるチケット種類における基本料金
case class BaseFee(
  // この価格が適用できる条件
  showConditions: Set[ShowCondition],
  fee: Money) {
  def availableFor(show: Show): Boolean =
    showConditions.forall(_.accepts(show))
}

case class Ticket(
  ticketType: TicketType,
  fee: Fee) {
  def totalFee: Money = fee.total
  def requiredIDCards: Set[IDCard] =
    ticketType.requiredIDCards
  def idCardRequired: Boolean =
    requiredIDCards.nonEmpty
}
object Ticket {
  def availableTicketsFor(show: Show, has3DGlass: Boolean): Seq[Ticket] =
    TicketType.all.flatMap { tt => tt.ticketFor(show, has3DGlass) }
}

// 上映日時。料金計算に必要な性質のみを保持するようにした。
case class DateTime(
  // 平日
  isWeekday: Boolean,
  // 映画の日(毎月1日)
  isCinemaDay: Boolean,
  // 20時以降
  isLate: Boolean) {
  // 土日祝
  def isHoliday: Boolean = !isWeekday
}

case class ShowType(
  // 爆音上映
  isLoudSound: Boolean,
  is3D: Boolean)

// 一回の上映
case class Show(
  showType: ShowType,
  // 開始日時
  startAt: DateTime) {
  def lateShowDiscountAvailable: Boolean =
    startAt.isLate && !showType.isLoudSound

  def isWeekday = startAt.isWeekday
  def isHoliday = startAt.isHoliday
  def isCinemaDay = startAt.isCinemaDay

  def isLoudSoundShow = showType.isLoudSound
  def is3DShow = showType.is3D
}

// 上映に対する条件
class ShowCondition(val accepts: Show => Boolean)
object ShowCondition {
  val weekday = new ShowCondition(_.isWeekday)
  val holiday = new ShowCondition(_.isHoliday)
  val lateShow = new ShowCondition(_.lateShowDiscountAvailable)
  val noLateShow = new ShowCondition(!_.lateShowDiscountAvailable)
  val cinemaDay = new ShowCondition(_.isCinemaDay)
}

// ユーザ条件を検証するために提示させる物品
sealed abstract class IDCard {
}
object IDCard {
  case object `身分証_70歳以上` extends IDCard
  // 大学・専門学校
  case object `学生証` extends IDCard
  // 中高生
  case object `生徒手帳` extends IDCard
  case object `障害者手帳` extends IDCard
}

// ユーザに対する条件
sealed abstract class UserCondition {
}
object UserCondition {
  case object `大学・専門学校` extends UserCondition
  case object `高校以下` extends UserCondition
  case object `中学・高校` extends UserCondition
  case object `学生以上` extends UserCondition
  case object `幼児・小学生` extends UserCondition
  case object `障がい者` extends UserCondition
  case object `70歳以上` extends UserCondition
  case object `60歳以上` extends UserCondition
  case object `シネマシティズン` extends UserCondition
  case object `エムアイカード` extends UserCondition
  case object `パーク80利用者` extends UserCondition
}
