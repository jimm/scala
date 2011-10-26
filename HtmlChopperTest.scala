import scala.testing.SUnit
import SUnit._

class HCTest(n: String) extends TestCase(n) {

  var chopper: HtmlChopper = new HtmlChopper(5) // Not XHTML strict
  var strict: HtmlChopper = new HtmlChopper(5, "...", true)

  override def runTest() = n match {
    case "closingTags" => {
      val openTags = List("p")
      var closeTags = List("p")
      assertEquals("", chopper.closingTags(openTags, closeTags))
      assertEquals("", strict.closingTags(openTags, closeTags))

      closeTags = List()
      assertEquals("", chopper.closingTags(openTags, closeTags))
      assertEquals("</p>", strict.closingTags(openTags, closeTags))
    }
    case "chopToMaxlenVisiblePlainText" => {
      assertEquals(null, chopper.chopToMaxlenVisible(null))
      assertEquals("", chopper.chopToMaxlenVisible(""))
      assertEquals("12345", chopper.chopToMaxlenVisible("12345"))

      assertEquals("1234", chopper.chopToMaxlenVisible("1234"))
      assertEquals("12345", chopper.chopToMaxlenVisible("12345"))
      assertEquals("12345", chopper.chopToMaxlenVisible("12345x"))
      assertEquals("12345", chopper.chopToMaxlenVisible("12345xx"))
    }
    case "chopToMaxlenVisibleHtml" => {
      assertEquals("<p>12345</p>", chopper.chopToMaxlenVisible("<p>12345</p>"))
      var longStr = "123<element>"
      assertEquals(longStr, chopper.chopToMaxlenVisible(longStr))
      longStr = "123<element><element2><!-- comment -->"
      assertEquals(longStr, chopper.chopToMaxlenVisible(longStr))
      assertEquals(longStr + "12",
		   chopper.chopToMaxlenVisible(longStr + "1234567"))
      longStr = "<longtag><another>" + "123"
      assertEquals(longStr, chopper.chopToMaxlenVisible(longStr))
    }
    case "closeTagsEmpty" => {
      assertEquals(null, chopper.closeTags(null))
      assertEquals("", chopper.closeTags(""))
    }
    case "closeTagsNothingToDo" => {
      assertEquals("abcde", chopper.closeTags("abcde"))
      assertEquals("<p>foobar</p>", chopper.closeTags("<p>foobar</p>"))
      assertEquals("<p>foo<i>bar</i></p>",
		   chopper.closeTags("<p>foo<i>bar</i></p>"))
    }
    case "closeTags" => {
      assertEquals("<p>12345", chopper.closeTags("<p>12345"))
      assertEquals("<p>12345</p>", strict.closeTags("<p>12345"))
      
      assertEquals("<p><span id=\"bc\">12345</span>",
		   chopper.closeTags("<p><span id=\"bc\">12345"))
      assertEquals("<p><span id=\"bc\">12345</span></p>",
		   strict.closeTags("<p><span id=\"bc\">12345"))

      assertEquals("<p>12345<br/>", chopper.closeTags("<p>12345<br/>"))
      assertEquals("<p>12345<br/></p>", strict.closeTags("<p>12345<br/>"))

      val html = "<br/><hr /><p>12345<br/>"
      assertEquals(html, chopper.closeTags(html))
      assertEquals(html + "</p>", strict.closeTags(html))
    }
    case "closeTagsCommentIgnored" => {
      assertEquals("<!-- comment -->12345",
		   chopper.closeTags("<!-- comment -->12345"))
    }
    case "empty" => {
      assertEquals(null, chopper.chop(null))
      assertEquals("", chopper.chop(""))
    }
    case "shortText" => {
      assertEquals("1234", chopper.chop("1234"))
      assertEquals("12345", chopper.chop("12345"))
    }
    case "plainText" => {
      assertEquals("12345", chopper.chop("12345"))
      assertEquals("12345...", chopper.chop("12345x"))
      assertEquals("12345...", chopper.chop("12345abcde"))
    }
    case "simple" => {
      assertEquals("<p>12345</p>", chopper.chop("<p>12345</p>"))
      assertEquals("<p>12345...", chopper.chop("<p>1234567890</p>"))
      assertEquals("<p>12345...</p>", strict.chop("<p>1234567890</p>"))

      val open = "<p><!-- comment --><span class=\"xxx\"><div id=\"foo\">"
      val close = "</div></span></p>"
      val longStr = open + "1234567890 and here is <b>more text</b>" + close
      assertEquals(open + "12345...</div></span>", chopper.chop(longStr))
      assertEquals(open + "12345..." + close, strict.chop(longStr))
    }
    case "strict" => {
      // balanced (but not a legal list)
      assertEquals("<ul><li>123<li>45...</li></li></ul>",
		   strict.chop("<ul><li>123<li>456<li>7890</ul>"))
      // non-strict will leave "li" tags alone
      assertEquals("<ul><li>123<li>45...</ul>",
		   chopper.chop("<ul><li>123<li>456<li>789</ul>"))
    }
  }
}

object HtmlChopperTest extends TestConsoleMain {
  override def suite = new TestSuite(
    new HCTest("closingTags"),
    new HCTest("chopToMaxlenVisiblePlainText"),
    new HCTest("chopToMaxlenVisibleHtml"),
    new HCTest("closeTagsEmpty"),
    new HCTest("closeTagsNothingToDo"),
    new HCTest("closeTags"),
    new HCTest("closeTagsCommentIgnored"),
    new HCTest("empty"),
    new HCTest("shortText"),
    new HCTest("plainText"),
    new HCTest("simple"),
    new HCTest("strict")
  )
}

val r = new TestResult()
HtmlChopperTest.suite.run(r)
for (val tf <- r.failures()) println(tf.toString())
if (r.failureCount == 0) println("OK: all tests passed")
