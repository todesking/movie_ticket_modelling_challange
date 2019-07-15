package mtmc.application

import mtmc.domain.Show
import mtmc.domain.DateTime
import mtmc.domain.Ticket

object TicketVendor {
  private[this] def readInt() = {
    print("> ")
    scala.io.StdIn.readLine().toInt
  }

  // 本来なら上映検索サービスからから本日分の未上映回を取得することになりそうだが、本質ではないので省略
  val shows = Seq(
    Show(DateTime(false, false, false), false),
    Show(DateTime(false, false, false), true),
    Show(DateTime(false, false, true), false),
    Show(DateTime(false, false, true), true),
    Show(DateTime(false, true, false), false),
    Show(DateTime(false, true, false), true),
    Show(DateTime(false, true, true), false),
    Show(DateTime(false, true, true), true),
    Show(DateTime(true, false, false), false),
    Show(DateTime(true, false, false), true),
    Show(DateTime(true, false, true), false),
    Show(DateTime(true, false, true), true),
    Show(DateTime(true, true, false), false),
    Show(DateTime(true, true, false), true),
    Show(DateTime(true, true, true), false),
    Show(DateTime(true, true, true), true))

  // これはアプリケーションの責務でええやろ
  def pretty(s: Show): String = {
    val date = if (s.isWeekday) "平日" else "土日祝"
    val dateEx = if (s.isCinemaDay) "かつ映画の日" else ""
    val time = if (s.startAt.isLate) "20時以降" else "20時以前"
    val loud = if (s.isLoudSoundShow) ", 爆音上映" else ""
    s"$date$dateEx, $time$loud"
  }

  def main(args: Array[String]): Unit = {
    println("これはチケット販売機です")
    println("購入したい回を選択してください")

    shows.zipWithIndex.foreach {
      case (show, i) =>
        println(f"${i + 1}%2d ${pretty(show)}")
    }
    val show = shows(readInt())
    println(s"${pretty(show)} を選択しました")

    println("購入するチケットを選んでください")
    val tickets = Ticket.availableTicketsFor(show)
    tickets.zipWithIndex.foreach {
      case (t, i) =>
        println(f"$i%2d ${t.ticketType.name} ${t.fee.value}%,d円")
    }
    val ticket = tickets(readInt())
    println(f"${ticket.ticketType.name} ${ticket.fee.value}%,d円 を選択しました")
  }
}
