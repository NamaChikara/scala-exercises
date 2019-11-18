
class Rational(n: Int, d: Int) {

  require(d != 0)

  val num: Int = n
  val den: Int = d

  var temp: Int = -1

  def lessThan(that: Rational): Boolean = {
    num * that.den < that.num * den
  }

  def max(that: Rational): Rational = {
    if (lessThan(that)) that else this
  }

}

val half = new Rational(1, 2)
val third = new Rational(1, 3)

third.lessThan(half)
half.lessThan(third)

val new_half = half max third

half.temp
new_half.temp
new_half.temp = -2
half.temp

case class RationalCase(n: Int, d: Int) {

  require(d != 0)

  val num: Int = n
  val den: Int = d

  var temp: Int = -1

  def lessThan(that: RationalCase): Boolean = {
    num * that.den < that.num * den
  }

  def max(that: RationalCase): RationalCase = {
    if (lessThan(that)) that else this.copy()
  }

}

val fourth = new RationalCase(1, 4)
val fifth = new RationalCase(1, 5)

val new_fourth = fourth max fifth

new_fourth.temp = -2
fourth.temp