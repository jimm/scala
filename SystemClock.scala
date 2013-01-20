// package utils

import java.util._
import java.util.regex._
import java.util.logging.Logger

/**
 * Keeper of the current system time. The time can be set, which is only used
 * for testing.
 * <p>
 * Also contains the {@link #parse} utility method for parsing timestamp
 * strings.
 * <p>
 * This class is not thread safe. However, the only time we'll be changing the
 * time is during testing.
 */
object SystemClock {

  val DT_PATTERN = Pattern.compile("(\\d{4})-(\\d{2})-(\\d{2})[Tt ](\\d{2}):(\\d{2}):(\\d{2})(\\.(\\d+))?([Zz]|([-+]\\d{2}:\\d{2}))")
  //         match groups             1          2        3           4        5        6      7     8   9     10

  var frozenTime: Date = _

  /**
   * Returns the current time. If time is frozen, returns that time else
   * returns the current wall clock time.
   */
  def now: Date = if (frozenTime == null) new Date() else frozenTime

  /**
   * Freezes time to a particular value <var>d</var>.
   */
  def freezeTime(d: Date) { frozenTime = d }

  /**
   * Unfreezes time. Subsequent calls to {@link #now} will return the
   * current wall clock time.
   */
  def unfreezeTime { frozenTime = null }

  /**
   * Parses timestamp strings and returns a Date. Timestamp strings are
   * are of the form "YYYY-MM-DDTHH:MM:SS[.sss](Z|{-+}hh:mm)" where
   * "T" is a literal character, the milliseconds part ".sss" is optional,
   * and the time zone offset is not optional but must be either "Z"
   * (signifying UTC) or "{+-}hh:mm". (This timestamp representation
   * is a very simplified version of
   * <a href="http://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a>.)
   */
  def parse(datetime: String): Date = {
    if (datetime == null || datetime.length == 0)
      return null

    val m = DT_PATTERN.matcher(datetime)
    if (!m.matches()) {
      Logger.getLogger("SystemClock").info("SystemClock.parse can not parse datetime string \"" + datetime + "\"")
      return null
    }

    val g9 = m.group(9).toUpperCase
    val tzString = g9 match {
      case "Z" => "GMT"
      case _ => "GMT" + g9
    }
    val tz = TimeZone.getTimeZone(tzString)

    val cal = Calendar.getInstance(tz)
    cal.set(Calendar.YEAR, m.group(1).toInt)
    cal.set(Calendar.MONTH, m.group(2).toInt - 1)
    cal.set(Calendar.DATE, m.group(3).toInt)
    cal.set(Calendar.HOUR_OF_DAY, m.group(4).toInt)
    cal.set(Calendar.MINUTE, m.group(5).toInt)
    cal.set(Calendar.SECOND, m.group(6).toInt)
    cal.set(Calendar.MILLISECOND, if (m.group(7) == null) 0 else m.group(8).toInt)
    cal.getTime
  }

  /**
   * Returns a String of the form "YYYY-MM-DDTHH:MM:SS.sssZ" where "T" is
   * a literal character, the milliseconds part ".sss" is optional, and
   * the time zone offset is "Z" (signifying UTC) or "{+-}hh:mm".
   * (This timestamp representation is a very simplified version of <a
   * href="http://en.wikipedia.org/wiki/ISO_8601">ISO 8601</a>.)
   * <p>
   * If <var>d</var> is <code>null</code>, returns <code>null</code>.
   */
  def toTS(d: Date): String = {
    if (d == null)
      return null

    val cal = Calendar.getInstance
    cal.setTime(d)
    cal.setTimeZone(TimeZone.getTimeZone("GMT"))
    String.format("%1$tY-%1$tm-%1$tdT%1$tH:%1$tM:%1$tS.%1$tLZ", cal)
  }
}
