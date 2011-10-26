package jimm.util
import java.io._
import org.scalatest.FunSuite

/**
 * Tests the base64 encoder/decoder.
 *
 * @author Jim Menard, <a href="mailto:jimm@io.com">jimm@io.com</a>
 * @see Base64
 */
class Base64Suite extends FunSuite {

  test("empty") {
    roundTrip("")
  }

  test("simple") {
    roundTrip("This is a test. It is only a test.")
  }

  test("binary") {
    // Don't bother trying 16-bit characters the String class messes up
    // the binary representations due to UTF-8 encoding.
    roundTrip("\u0000\u0001\u0002\u00ff\u00dc")
  }

  def roundTrip(text:String) {
    // Encode string
    var in = new ByteArrayInputStream(text.getBytes())
    var out = new ByteArrayOutputStream()
    try {
      Base64.encode(in, out)
    }
    catch {
      case ioe: IOException => fail(ioe.toString())
    }

    // Test encoded string
    var encoded = out.toString()
    if (text.length() > 0)
      assert(encoded.length() > text.length())

    // Decode
    in = new ByteArrayInputStream(encoded.getBytes())
    out = new ByteArrayOutputStream()
    try {
      Base64.decode(in, out)
    }
    catch {
      case ioe: IOException => fail(ioe.toString())
    }

    // Compare decoded with original
    val roundTripResult = out.toString()
    assert(roundTripResult === text)
  }

  test("spaces in encoding") {
    val text = "This is a test. It is only a test."
    // Encode string
    var in = new ByteArrayInputStream(text.getBytes())
    var out = new ByteArrayOutputStream()
    try {
      Base64.encode(in, out)
    }
    catch {
      case ioe: IOException => fail(ioe.toString())
    }

    // Test encoded string
    var encoded = out.toString()
    if (text.length() > 0)
      assert(encoded.length() > text.length())

    // Add whitespace to beginning, middle, and end of string. They should be
    // ignored
    encoded = "\n   " + encoded.substring(0, 5) + "\n   " +
      encoded.substring(5) + "  \n    "

    // Decode
    in = new ByteArrayInputStream(encoded.getBytes())
    out = new ByteArrayOutputStream()
    try {
      Base64.decode(in, out)
    }
    catch {
      case ioe: IOException => fail(ioe.toString())
    }

    // Compare decoded with original
    val roundTripResult = out.toString()
    assert(text === roundTripResult)
  }

}
