package mtmc.domain

// チケットの種類(一般、学生、シニアetc)
// userConditions は BaseFee に持たせたほうが柔軟だが、
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
  def ticketFor(show: Show, has3DGlass: Boolean): Option[Ticket] = {
    // 3Dグラス所持の有無を受けとるのではなく、汎用的なExtraFee計算ポリシーを渡すことでより柔軟になる。
    // が、現段階でそこまでするのはoverabstractionだと判断した。
    val candidates = baseFees.filter(_.availableFor(show))
    if (candidates.isEmpty) {
      None
    } else {
      val chosen = candidates.toSeq.map { base =>
        Ticket(this, Fee(base.fee, calcExtraFees(show, has3DGlass)))
      }.minBy(_.totalFee)
      Some(chosen)
    }
  }
  def calcExtraFees(show: Show, has3DGlass: Boolean): Seq[ExtraFee] = {
    // レイトショーや映画の日等も同様にExtraFeeとして処理することができるかもしれない。
    // しかし、それらはシネマシティのチケット料金ページでは「追加料金」
    // という見せ方をしていないので、別々のbaseFeeとして表現した。
    val extras = scala.collection.mutable.ArrayBuffer[ExtraFee]()
    if (show.is3DShow) {
      extras += ExtraFee("3D上映", Money(400))
      if (has3DGlass)
        extras += ExtraFee("3Dメガネ持参", Money(-100))
    }
    extras.toSeq
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

