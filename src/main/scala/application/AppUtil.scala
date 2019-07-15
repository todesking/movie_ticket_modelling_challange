package mtmc.application

import mtmc.domain.Show
import mtmc.domain.DateTime

object AppUtil {
  def readInt() = {
    print("> ")
    scala.io.StdIn.readLine().toInt
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
    val item = items(readInt())
    println(s"${format(item)} を選択しました")
    item
  }

  // 本来なら上映検索サービスからから本日分の未上映回を取得することになりそうだが、本質ではないので省略
  val availableShows = Seq(
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

}
