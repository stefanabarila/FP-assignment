package dataset

import dataset.util.Commit.Commit

import java.text.SimpleDateFormat
import java.util.SimpleTimeZone
import scala.math.Ordering.Implicits._

/**
 * Use your knowledge of functional programming to complete the following functions.
 * You are recommended to use library functions when possible.
 *
 * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
 * When asked for dates, use the `commit.commit.committer.date` field.
 *
 * This part is worth 40 points.
 */
object Dataset {


  /** Q23 (4p)
   * For the commits that are accompanied with stats data, compute the average of their additions.
   * You can assume a positive amount of usable commits is present in the data.
   *
   * @param input the list of commits to process.
   * @return the average amount of additions in the commits that have stats data.
   */
  def avgAdditions(input: List[Commit]): Int = {
    val additions = input.flatMap(_.stats).map(_.additions)
    if (additions.isEmpty) 0
    else additions.sum / additions.size
  }

  /** Q24 (4p)
   * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
   * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
   * NB!filename of a file is always defined.
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   *
   * @param input list of commits to process.
   * @return the hour and the amount of files changed during this hour.
   */
  def jsTime(input: List[Commit]): (Int, Int) = {
    val sdf = new SimpleDateFormat("HH")
    sdf.setTimeZone(new SimpleTimeZone(0, "UTC"))
    val hourlyJsCounts = input.map { c =>
      val hour = sdf.format(c.commit.committer.date).toInt
      val jsFilesCount = c.files.count(file => file.filename.exists(_.endsWith(".js")))
      (hour, jsFilesCount)
    }
    val groupedByHour = hourlyJsCounts.groupBy(_._1)
    val totalJsPerHour = groupedByHour.map { case (hour, counts) =>
      val totalFiles = counts.map(_._2).sum
      (hour, totalFiles)
    }
    totalJsPerHour.maxBy(_._2)
  }


  /** Q25 (5p)
   * For a given repository, output the name and amount of commits for the person
   * with the most commits to this repository.
   * For the name, use `commit.commit.author.name`.
   *
   * @param input the list of commits to process.
   * @param repo  the repository name to consider.
   * @return the name and amount of commits for the top committer.
   */
  def topCommitter(input: List[Commit], repo: String): (String, Int) = {
    val a = input.filter(c => c.url.contains(repo)).map(c => c.commit.author.name)
    a.groupBy(n => n).map{case (a, b) => (a, b.size)}.maxBy(_._2)
  }

  /** Q26 (9p)
   * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
   * Leave out all repositories that had no activity this year.
   *
   * @param input the list of commits to process.
   * @return a map that maps the repo name to the amount of commits.
   *
   *         Example output:
   *         Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
   */
  def commitsPerRepo(input: List[Commit]): Map[String, Int] = {
    val sdf = new SimpleDateFormat("yyyy")
    sdf.setTimeZone(new SimpleTimeZone(0, "UTC"))
    val a = input.filter{c =>
      val y = sdf.format(c.commit.committer.date).toInt
      y == 2019}

    a.groupBy{c =>
      val parts = c.url.split("/")
      s"${parts(4)}/${parts(5)}"}
      .map{case (a, b) => (a, b.size)}
  }


  /** Q27 (9p)
   * Derive the 5 file types that appear most frequent in the commit logs.
   * NB!filename of a file is always defined.
   * @param input the list of commits to process.
   * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
   */
  def topFileFormats(input: List[Commit]): List[(String, Int)] = {
    val types = input.flatMap(c => c.files).flatMap(c => c.filename).map(c => c.split("\\.").last)
    types.groupBy(identity).map{case (a, b) => (a, b.size)}.toList.sortWith((x, y) => x._2 > y._2).take(5)
  }


  /** Q28 (9p)
   *
   * A day has different parts:
   * morning 5 am to 12 pm (noon)
   * afternoon 12 pm to 5 pm.
   * evening 5 pm to 9 pm.
   * night 9 pm to 4 am.
   *
   * Which part of the day was the most productive in terms of commits ?
   * Return a tuple with the part of the day and the number of commits
   *
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   */
  def mostProductivePart(input: List[Commit]): (String, Int) = {
    val sdf = new SimpleDateFormat("HH")
    sdf.setTimeZone(new SimpleTimeZone(0, "UTC"))

    def getPartOfDay(hour: Int): String = {
      if (hour >= 5 && hour < 12) "morning"
      else if (hour >= 12 && hour < 17) "afternoon"
      else if (hour >= 17 && hour < 21) "evening"
      else "night"
    }

    val dayParts = input.map { c =>
      val hour = sdf.format(c.commit.committer.date).toInt
      getPartOfDay(hour)
    }

    dayParts
      .groupBy(identity)
      .map { case (part, list) => (part, list.size) }
      .maxBy(_._2)
  }
}
