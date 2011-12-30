import static org.junit.Assert._
import java.util.{Calendar, Date}
import org.scalatest.FunSuite
import org.scalatest.BeforeAndAfter
import utils.SystemClock

class SystemClockTest extends FunSuite with BeforeAndAfter {

  var Calendar cal

  before {
    val gmt = TimeZone.getTimeZone("GMT")
    cal = Calendar.getInstance(gmt)
    cal.set(Calendar.YEAR, 2009)
    cal.set(Calendar.MONTH, 7) // August
    cal.set(Calendar.DATE, 24)
    cal.set(Calendar.HOUR_OF_DAY, 14)
    cal.set(Calendar.MINUTE, 24)
    cal.set(Calendar.SECOND, 55)
    cal.set(Calendar.MILLISECOND, 123)
  }

  after {
    SystemClock.unfreezeTime
  }

  test("no frozen time") {
    val d = new Date
    assert(SystemClock.now().getTime() - d.getTime() < 100)
  }
  
  test("frozen time") {
    val d = new Date
    d.setTime(42)
    SystemClock.freezeTime(d)
    assert(d === SystemClock.now)
  }
  
  test("freezing with calendar") {
    SystemClock.freezeTime(cal)
    assert(cal.getTime === SystemClock.now)
  }
  
  test("freezing with string") {
    SystemClock.freezeTime("2009-08-24T14:24:55.123Z")
    assert(cal.getTime === SystemClock.now)
  }

  test("melting") {
    val testStart = new Date

    val d = cal.getTime
    SystemClock.freezeTime(d)
    assert(d === SystemClock.now)

    SystemClock.unfreezeTime
    assert(SystemClock.now.getTime - testStart.getTime < 100)
  }

  test("get frozen calendar instance") {
    val d = new Date
    SystemClock.freezeTime(d)
    assert(d === SystemClock.getCalendarInstance().getTime)
  }

  test("parse") {
    val ms = cal.getTimeInMillis()

    // with milliseconds
    var pd = SystemClock.parse("2009-08-24T14:24:55.123Z")
    assertNotNull(pd)
    assert(ms === pd.getTime)

    // without milliseconds
    pd = SystemClock.parse("2009-08-24T14:24:55Z")
    assertNotNull(pd)
    assert(ms - 123 === pd.getTime)

    // GMT as "+00:00"
    pd = SystemClock.parse("2009-08-24T14:24:55.123+00:00")
    assertNotNull(pd)
    assert(ms === pd.getTime)

    // Different time zone
    val laterPd = SystemClock.parse("2009-08-24T14:24:55.123+00:30")
    assertNotNull(laterPd)
    assert(ms - (30L*60L*1000L) === laterPd.getTime)
  }

  test("parse bad strings") {
    assertNull(SystemClock.parse("bad date"))
  }
  
  test("no time zone is null") {
    assertNull(SystemClock.parse("2009-08-24T14:24:55.123"))
  }

  test("toTS") {
    val s = "2009-08-24T14:24:55.123Z"
    val pd = SystemClock.parse(s)
    assert(s === SystemClock.toTS(pd))

    assert(null === SystemClock.toTS(null))
  }
}
