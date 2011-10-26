package jimm.util
import java.io._

/**
 * This class provides base64 encoding and decoding services.
 *
 * @author Jim Menard, <a href="mailto:jimm@io.com">jimm@io.com</a>
 */
object Base64 {

  val LineLen = 60
  val EOL = "\r\n".getBytes()

  val encodeTable:Array[Byte] = new Array[Byte](64)
  val decodeTable:Array[Byte] = new Array[Byte](256)

  for (i <- 0 until 26) {
    encodeTable(i) = ('A' + i).toByte
    encodeTable(26 + i) = ('a' + i).toByte
  }
  for (i <- 0 until 10)
    encodeTable(52 + i) = ('0' + i).toByte
  encodeTable(62) = '+'
  encodeTable(63) = '/'

  // Fill decode table with 0x80. We don't use this value we could use
  // it to check for illegal input bytes: an input byte is illegal if
  // decodeTable(input_byte) == 0x80.
  for (i <- 0 to 255)
    decodeTable(i) = 0x80.toByte
  for (i <- 0 until 26) {
    decodeTable('A' + i) = i.toByte
    decodeTable('a' + i) = (26 + i).toByte
  }
  for (i <- 0 until 10)
    decodeTable('0' + i) = (52 + i).toByte
  decodeTable('+') = 62.toByte
  decodeTable('/') = 63.toByte
  decodeTable('=') = 0.toByte

  def encode(in:InputStream, out:OutputStream) {
    val inBytes:Array[Byte] = new Array[Byte](3)
    val outBytes:Array[Byte] = new Array[Byte](4)
    var linelen = 0
    var inLen = in.read(inBytes)
    while (inLen > 0) {
      // If we read fewer than three Bytes, pad the rest with '\0'.
      for (i <- inLen until 3)
	inBytes(i) = '\0'

      // Encode three input Bytes as four output Bytes.
      outBytes(0) = encodeTable((inBytes(0) >> 2) & 0x3f)
      outBytes(1) = encodeTable(((inBytes(0) & 0x03) << 4) + ((inBytes(1) >> 4) & 0x0f))
      outBytes(2) = encodeTable(((inBytes(1) & 0x0f) << 2) + ((inBytes(2) >> 6) & 0x03))
      outBytes(3) = encodeTable(inBytes(2) & 0x3f)

      // Pad with magic '=' Byte when fewer than three Bytes were read.
      if (inLen < 3) {
	outBytes(3) = '='
	if (inLen < 2)
	  outBytes(2) = '='
      }

      linelen += 4
      if (linelen >= LineLen) {
	out.write(EOL)
	linelen = 0
      }
      out.write(outBytes)
      inLen = in.read(inBytes)
    }
    out.write(EOL)
  }

  def decode(in:InputStream, out:OutputStream) {

    val inBytes:Array[Byte] = new Array[Byte](4)
    val decodedBytes:Array[Byte] = new Array[Byte](4)
    val outBytes:Array[Byte] = new Array[Byte](3)

    def readEncodedBlock(): Int = {
      var inLen = 0
      while (inLen < 4) {
        var c = in.read()
        if (c == -1)
	  return inLen
        if (c > ' ') {            // Ignore whitespace and other illegal Bytes
	  inBytes(inLen) = c.toByte
          inLen += 1
        }
      }
      return 4
    }

    while (readEncodedBlock() > 0) {
      for (i <- 0 to 3)
	decodedBytes(i) = decodeTable(inBytes(i))

      outBytes(0) = ((decodedBytes(0) << 2) + ((decodedBytes(1) >> 4) & 0x03)).toByte
      outBytes(1) = (((decodedBytes(1) & 0x0f) << 4) + ((decodedBytes(2) >> 2) & 0x0f)).toByte
      outBytes(2) = (((decodedBytes(2) & 0x03) << 6) + (decodedBytes(3) & 0x3f)).toByte

      val len = if (inBytes(2) == '=') 1 else if (inBytes(3) == '=') 2 else 3
      out.write(outBytes, 0, len)
    }
  }

}
