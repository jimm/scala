import java.util.regex._

object HtmlChopper {
  /**
   * Regular expression for finding (X)HTML elements.
   * <p>
   * 1 = opening slash, 2 = tag name, 3 = remainder, 4 = trailing slash
   */
  val TAG_REGEX = "<(/?)([^ \t\n\r/>]+)([^/>]*)(/?)>"

  /** Set of HTML elements that do not need closing tags. */
  val OK_IF_UNBALANCED = List("p", "br", "hr", "img", "input", "li")
}

/**
 * Chops HTML to a maximum display length. Length does not include (X)HTML
 * elements. Any open elements are closed. If chopped, then '...' (or whatever
 * else you choose) is appended to the chopped text. By default, HTML tags
 * that do not require closing tags are not closed.
 * <p>
 * The {@link #closeTags} method is also useful by itself. See its comment.
 *
 * @author Jim Menard, <a href="mailto:jim@iamplify.com">jim@iamplify.com</a>
 */
class HtmlChopper(val maxlen: Int, val chopSuffix: String,
		  val xhtmlStrict: Boolean)
{
  assume(maxlen >= 0)

  def this(maxlen: Int) = this(maxlen, "...", false)
  def this(maxlen: Int, chopSuffix: String) = this(maxlen, chopSuffix, false)

  /**
   * Returns (XH)HTML chopped off to maxlen visible characters, followed by
   * <var>chopSuffix</var>. The text returned may be longer than maxlen
   * because (X)HTML elements aren't counted towards the max length. Any open
   * tags are closed.
   */
  def chop(html: String): String = {
    if (html == null || html.length <= maxlen)
      return html

    val choppedHtml = chopToMaxlenVisible(html)
    if (html == choppedHtml)
      html
    else
      closeTags(choppedHtml + chopSuffix)
  }

  /**
   * Returns the first maxlen visible chars, plus any tags immediately
   * following the last visible char.
   */
  def chopToMaxlenVisible(html: String): String = {
    if (html == null || html.length() <= maxlen)
      return html

    var visibleLen = 0
    val len = html.length
    var inTag = false
    var i = 0
    while (i < len && visibleLen < maxlen) {
      val c = html(i)
      if (inTag) {
	if (c == '>') inTag = false
      }
      else {
	if (c == '<') inTag = true
	else visibleLen += 1
      }
      i += 1
    }
    if (i < len && html(i) == '<') {
      while (i < len && html(i) != '>')
	i += 1
      if (i < len) i += 1
    }
    html.substring(0, i)
  }

  /**
  * Returns (HX)HTML with all open tags closed. This method assumes that the
  * tags in the text it is processing are not malformed. For example, you can
  * pass it "&ltspan&gtabc" and it will close the span, but you can't pass
  * it "&ltspa".
  * <p>
  * NOTE: this code assumes that tags are well-balanced.
  */
  def closeTags(html: String): String = {
    if (html == null || html.length == 0)
      return html

    // 1 = opening slash, 2 = tag name, 3 = remainder, 4 = trailing slash
    val p = Pattern.compile(HtmlChopper.TAG_REGEX)
    val m = p.matcher(html)
    var openTags: List[String] = List()
    var closeTags: List[String] = List()
    while (m.find()) {
      val tag = m.group(2)
      if ("/" == m.group(1))
	closeTags = tag :: closeTags
      else if ("/" != m.group(4) &&	  // skip self-closing tags and
	       tag(0) != '!')		  // comments
	openTags = tag :: openTags
    }
	      
    html + closingTags(openTags, closeTags)
  }

  /**
   * Returns a string containing all unclosed tags in the proper order.
   * If xhtmlStrict is false, ignores tags in <code>OK_IF_UNBALANCED</code>
   * like "br", "hr", and "li".
   */
  def closingTags(openTags: List[String], closeTags: List[String]) = {
    val open =
      if (xhtmlStrict) openTags
      else openTags.filter(!HtmlChopper.OK_IF_UNBALANCED.contains(_))
    val closed =
      if (xhtmlStrict) closeTags
      else closeTags.filter(!HtmlChopper.OK_IF_UNBALANCED.contains(_))
    val openLen = open.length
    val closedLen = closed.length
    if (openLen > closedLen) {
      var unclosed: List[String] = List()
      for (i <- closedLen to (openLen - 1))
	unclosed = open(i) :: unclosed
      unclosed.reverse.mkString("</", "></", ">")
    }    
    else
      ""
  }
}
