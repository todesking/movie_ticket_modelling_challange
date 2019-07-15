package mtmc.application

import mtmc.domain.Ticket

object TicketVendor {
  def main(args: Array[String]): Unit = {
    println("これはチケット販売機です")
    println("購入したい回を選択してください")

    val show = AppUtil.choice(AppUtil.availableShows)(AppUtil.pretty)

    println("購入するチケットを選んでください")
    val tickets = Ticket.availableTicketsFor(show)
    val ticket =
      AppUtil.choice(tickets) { t =>
        val idRequirement =
          if (t.idCardRequired) t.requiredIDCards.toSet.mkString(" (", ", ", "をご用意ください)")
          else ""
        f"${t.ticketType.name} ${t.fee.total.toInt}%,d円$idRequirement"
      }
    println("お会計")
    println(f"* 基本料金 ${ticket.fee.base.toInt}%,d")
    ticket.fee.extras.foreach { ex =>
      println(f"* ${ex.name} ${ex.amount.toInt}%,d")
    }
    println(f"合計 ${ticket.fee.total.toInt}%,d")
  }
}
