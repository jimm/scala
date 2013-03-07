import scala.language.implicitConversions

class Rational(n: Int, d: Int) {

  private def gcd(a: Int, b: Int): Int = 
    if (b == 0) a else gcd(b, a % b) 
  private val g = gcd(n, d) 
  val numer: Int = n / g 
  val denom: Int = d / g 

  def this(n: Int) = this(n, 1)

  def +(that: Rational): Rational = 
    new Rational(numer * that.denom + that.numer * denom, 
		 denom * that.denom) 
  def -(that: Rational): Rational = 
    new Rational(numer * that.denom - that.numer * denom, 
		 denom * that.denom) 
  def *(that: Rational): Rational = 
    new Rational(numer * that.numer, denom * that.denom) 
  def /(that: Rational): Rational = 
    new Rational(numer * that.denom, denom * that.numer) 

  def +(that: Int): Rational = this + new Rational(that)
  def -(that: Int): Rational = this - new Rational(that)
  def *(that: Int): Rational = this * new Rational(that)
  def /(that: Int): Rational = this / new Rational(that)

  override def toString(): String = {
    var intPart = 0
    var n = numer
    if (numer > denom) {
      intPart = numer / denom
      n = numer % denom
    }
    var str = ""
    if (intPart > 0) str += (intPart + " ")
    if (intPart == 0 && n == 0)
      "0"
    else if (n == 0)
      str
    else
      str + n + "/" + denom
  }
}

// Allow 2 * someRational through implicit conversion
implicit def intToRational(x: Int): Rational = new Rational(x)

val r = new Rational(1, 2)
println(r + new Rational(3, 4))
println(r + 14)
println(41 + new Rational(1))
