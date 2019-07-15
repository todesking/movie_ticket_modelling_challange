package mtmc.domain

case class Money(toInt: Int) {
  def +(rhs: Money): Money =
    Money(toInt + rhs.toInt)
}
case object Money {
  implicit val moneyOrdering: Ordering[Money] =
    Ordering.by(_.toInt)
}

// チケットの種類(一般、学生、シニアetc)
// userConditions は FeeCandidate に持たせたほうが柔軟だが、
// それが必要なチケット種類を作ることはないでしょうと判断した。
case class TicketType(
  name: String,
  // このチケットが対象とするユーザ
  userConditions: Set[UserCondition],
  // 要求される身分証。
  // UserConditionに持たせてもいいかもしれないが、チケット定義の柔軟性・可読性を考えた結果こうなった。
  // 今でもシネマシティズン(60歳以上)は身分証不要だが
  // シニアは身分証必要などがあるので……
  requiredIDCards: Set[IDCard],
  // チケットの料金。条件の組み合わせにより複数の価格設定がある
  baseFees: Set[BaseFee]) {
  def ticketFor(show: Show): Option[Ticket] = {
    val candidates = baseFees.filter(_.availableFor(show))
    if (candidates.isEmpty) {
      None
    } else {
      val chosen = candidates.toSeq.map { base =>
        Ticket(this, Fee(base.fee, Seq()))
      }.minBy(_.totalFee)
      Some(chosen)
    }
  }
}

object TicketType {
  import mtmc.domain.{ UserCondition => U }
  import mtmc.domain.{ ShowCondition => S }

  private[this] var _all: Seq[TicketType] = Vector()
  def all = _all

  private[this] def make(name: String)(ucs: UserCondition*)(ids: IDCard*)(candidates: (Set[ShowCondition], Int)*): TicketType = {
    val tt = TicketType(
      name,
      Set(ucs: _*),
      Set(ids: _*),
      candidates.map {
        case (scs, fee) =>
          BaseFee(scs, Money(fee))
      }.toSet)
    _all = _all :+ tt
    tt
  }

  private[this] def feeInShow(fee: Int)(scs: ShowCondition*): (Set[ShowCondition], Int) =
    (Set(scs: _*), fee)

  val `シネマシティズン` = make("シネマシティズン")(U.`シネマシティズン`)()(
    feeInShow(1000)(S.weekday, S.noLateShow),
    feeInShow(1000)(S.weekday, S.lateShow),
    feeInShow(1300)(S.holiday, S.noLateShow),
    feeInShow(1000)(S.holiday, S.lateShow),
    feeInShow(1100)(S.cinemaDay))
  val `シネマシティズン(60歳以上)` = make("シネマシティズン(60歳以上)")(U.`シネマシティズン`, U.`60歳以上`)()(
    feeInShow(1000)())
  val `一般` = make("一般")()()(
    feeInShow(1800)(S.weekday, S.noLateShow),
    feeInShow(1300)(S.weekday, S.lateShow),
    feeInShow(1800)(S.holiday, S.noLateShow),
    feeInShow(1300)(S.holiday, S.lateShow),
    feeInShow(1100)(S.cinemaDay))
  val `シニア` = make("シニア(70歳以上)")(U.`70歳以上`)(IDCard.`身分証_70歳以上`)(
    feeInShow(1100)())
  val `学生(大・専)` = make("学生(大・専)")(U.`大学・専門学校`)(IDCard.`学生証`)(
    feeInShow(1500)(S.weekday, S.noLateShow),
    feeInShow(1300)(S.weekday, S.lateShow),
    feeInShow(1500)(S.holiday, S.noLateShow),
    feeInShow(1300)(S.holiday, S.lateShow),
    feeInShow(1100)(S.cinemaDay))
  val `中・高校生` = make("中・高校生")(U.`中学・高校`)(IDCard.`生徒手帳`)(
    feeInShow(1000)())
  val `幼児・小学生` = make("幼児（3才以上）・小学生")(U.`幼児・小学生`)()(
    feeInShow(1000)())
  val `障がい者(学生以上)` = make("障がい者（学生以上）")(U.`障がい者`, U.`学生以上`)(IDCard.`障害者手帳`)(
    feeInShow(1000)())
  val `障がい者(高校以下)` = make("障がい者（高校以下）")(U.`障がい者`, U.`高校以下`)(IDCard.`障害者手帳`)(
    feeInShow(900)())
  val `エムアイカード` = make("エムアイカード")(U.`エムアイカード`)()(
    feeInShow(1600)(S.weekday, S.noLateShow),
    feeInShow(1300)(S.weekday, S.lateShow),
    feeInShow(1600)(S.holiday, S.noLateShow),
    feeInShow(1300)(S.holiday, S.lateShow))
  val `パーク80` = make("駐車場パーク80割引")(U.`パーク80利用者`)()(
    feeInShow(1400)(S.weekday, S.noLateShow),
    feeInShow(1100)(S.weekday, S.lateShow),
    feeInShow(1400)(S.holiday, S.noLateShow),
    feeInShow(1100)(S.holiday, S.lateShow))
}

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
  def availableTicketsFor(show: Show): Seq[Ticket] =
    TicketType.all.flatMap { tt => tt.ticketFor(show) }
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
