package meetup

package object monoid {

  // add a bunch of things together


  // fails: needs complex wrapper everytime, how to get 0
  trait NumberLike[A] {
    def +(to: A): A
  }

  class IntNumber(val underlying: Int) extends NumberLike[IntNumber] {
    def +(to: IntNumber): IntNumber = new IntNumber(underlying + to.underlying)
  }
  class DoubleNumber(val underlying: Double) extends NumberLike[DoubleNumber] {
    def +(to: DoubleNumber): DoubleNumber = new DoubleNumber(underlying + to.underlying)
  }

  def sumNumber[A <: NumberLike[A]](xs: List[A])(zero: A): A = xs match {
    case x :: rest => x + sumNumber(rest)(zero)
    case Nil       => zero
  }

  def sum(xs: List[Int]): Int =
    sumNumber(xs map (new IntNumber(_)))(new IntNumber(0)).underlying
  def sum(xs: List[Double]): Double =
    sumNumber(xs map (new DoubleNumber(_)))(new DoubleNumber(0)).underlying
}
