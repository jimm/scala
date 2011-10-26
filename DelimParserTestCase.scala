import junit.framework.TestCase 
import junit.framework.Assert.assertEquals 
import junit.framework.Assert.fail 
import scala.io.Source

object LineType extends Enumeration {
  val Data  = Value
  val Results = Value
}

class DelimParserTestCase extends TestCase {

  def testUsingDataFile() {
    for (line <- Source.fromFile("delim_parser_test_data.txt").getLines if line.length > 0 && line.charAt(0) != '#') {
      println(line)
    }
  }

}

