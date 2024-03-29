import java.io.Reader

/**
 * Parses delimited data. Handles quotes and embedded delimiters.
 * <p>
 * The caller must close the input Reader.
 *
 * @author Jim Menard, <a href="mailto:jimm@io.com">jimm@io.com</a>
 */
class DelimParser(in: Reader, delimiter: Char) {

  val EOF = -1

  var pushbackChar = EOF

  /**
   * Constructor, using ',' as the delimiter. The caller must close
   * <var>in</var>.
   *
   * @param in input reader
   */
  def this(in: Reader) = this(in, ',')

  /**
   * Returns an array of column data or <code>null</code> if there is no more
   * data. Handles delimiters and quotes within the data just as they are
   * generated by Excel comma- and tab-separated files.
   *
   * @return a <code>List</code> of strings; return <code>null</code> if
   * there is no more data.
   */
  def parse(): List[String] = {
    var columns = List[String]()
    var insideQuotes = false
    var numQuotesSeen = 0
    var buf = new StringBuffer()
    var prevChar = '\0'

    var charAsInt = nextChar()
    while (charAsInt != EOF) {
      var c = charAsInt.toChar
      val canonicalChar = c match {
        case '\r' => '\n'
        case _ => c
      }

      canonicalChar match {
        case '"' => {                   // Quote character
          if (!insideQuotes) {          // Start of quoted column
            insideQuotes = true
            numQuotesSeen = 0
          }
          else if (insideQuotes) {      // Inside quoted column
            if (numQuotesSeen == 1) {   // This is second of doubled quotes
              buf.append(c)
              numQuotesSeen = 0
            }
            else
              numQuotesSeen = 1
          }
        }
        case '\n' => {                  // Linefeed/newline
          if (insideQuotes) {
            if (numQuotesSeen == 1) {   // Closing quote at end of line
              return (buf.toString :: columns).reverse
            }
            else
              buf.append(c)
          }
          else {                        // End of line; return columns
            // Handle DOS line endings
            if (c == '\r')        { // Check for following '\n
              charAsInt = nextChar()
              c = charAsInt.toChar
              if (c != '\n')        // Eat following '\n' if it exists
                pushback(charAsInt) // Else put it back
            }

            charAsInt = nextChar()
            c = charAsInt.toChar
            if (columns.length == 0 && buf.length() == 0 && charAsInt == EOF)
              return null // Empty line at end of file

            pushback(charAsInt)
            return (buf.toString() :: columns).reverse
          }
        }
        case _ => {
          if (c == delimiter) {         // Normal delimiter
            if (!insideQuotes) {
              columns = buf.toString :: columns
              buf = new StringBuffer()
            }
            else {                   // Inside quoted column
              // Delimiter at end of quoted column data
              if (numQuotesSeen == 1) {
                insideQuotes = false
                columns = buf.toString :: columns
                buf = new StringBuffer()
              }
              // Delimiter inside quoted column
              else
                buf.append(delimiter)
            }
          }
          else {                // Everything else
            numQuotesSeen = 0
              buf.append(c)
          }
        }
      }

      prevChar = c
      charAsInt = nextChar()
    }

    // We've reached EOF
    if (columns.length == 0 && buf.length() == 0) // Empty line at end of file
      return null

    if (buf.length() > 0 || prevChar == delimiter) {
      columns = buf.toString :: columns
    }
    if (columns.length == 0) null else columns.reverse
  }

  def nextChar(): Int = {
    if (pushbackChar == EOF)
      in.read()
    else {
      val c = pushbackChar
      pushbackChar = EOF
      c
    }
  }

  def pushback(charAsInt: Int): Unit = { pushbackChar = charAsInt }

}
