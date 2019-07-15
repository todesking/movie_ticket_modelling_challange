package mtmc.application

import mtmc.domain.Show
import mtmc.domain.ShowType
import mtmc.domain.DateTime

object AppUtil {
  def readInt() = {
    print("> ")
    scala.io.StdIn.readLine().toInt
  }

  def confirm(msg: String): Boolean = {
    print(msg + "[y/n]")
    scala.io.StdIn.readLine().trim match {
      case "y" => true
      case "n" => false
      case _ => confirm(msg)
    }
  }

  def choiceShow(shows: Seq[Show]): Show = {
    val show = choice(shows)(pretty)
    show
  }

  def choice[A](items: Seq[A])(format: A => String = { x: A => x.toString }): A = {
    items.zipWithIndex.foreach {
      case (show, i) =>
        println(f"${i + 1}%2d ${format(show)}")
    }
    val item = items(readInt() - 1)
    println(s"${format(item)} を選択しました")
    item
  }

  // 本来なら上映検索サービスからから本日分の未上映回を取得することになりそうだが、本質ではないので省略
  val availableShows = {
    val b = Seq(false, true)
    for {
      isLoud <- b
      is3D <- b
      isWeekday <- b
      isCinemaDay <- b
      isLate <- b
    } yield Show(
      ShowType(isLoudSound = isLoud, is3D = is3D),
      DateTime(isWeekday = isWeekday, isCinemaDay = isCinemaDay, isLate = isLate))
  }

  // これはアプリケーションの責務でええやろ
  def pretty(s: Show): String = {
    val date = if (s.isWeekday) "平日" else "土日祝"
    val dateEx = if (s.isCinemaDay) "かつ映画の日" else ""
    val time = if (s.startAt.isLate) "20時以降" else "20時以前"
    val loud = if (s.isLoudSoundShow) ", 爆音上映" else ""
    val threeD = if (s.is3DShow) ", 3D" else ""
    s"$date$dateEx, $time$loud$threeD"
  }

}
