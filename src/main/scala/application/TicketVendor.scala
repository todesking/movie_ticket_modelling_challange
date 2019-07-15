package mtmc.application

import mtmc.domain.Ticket

object TicketVendor {
  import AppUtil.{readInt, pretty}

  val shows = AppUtil.availableShows

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
        val idRequirement =
          if (t.idCardRequired) ""
          else t.requiredIDCards.toSet.mkString(" (", ", ", "をご用意ください)")
        println(f"$i%2d ${t.ticketType.name} ${t.fee.value}%,d円$idRequirement")
    }
    val ticket = tickets(readInt())
    println(f"${ticket.ticketType.name} ${ticket.fee.value}%,d円 を選択しました")
    if (ticket.idCardRequired) {
      println("入場の再に以下をご提示ください")
      ticket.requiredIDCards.foreach { id =>
        println(s"* $id")
      }
    }
  }
}
