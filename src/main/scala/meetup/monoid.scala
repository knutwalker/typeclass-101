package meetup

package object monoid {

  // add a bunch of things together


  trait NumberLike[A] {
    def zero: A
    def add(x: A, y: A): A
  }

  implicit object IntNumber extends NumberLike[Int] {
    def zero: Int = 0
    def add(x: Int, y: Int): Int = x + y
  }
  implicit object DoubleNumber extends NumberLike[Double] {
    def zero: Double = 0.0
    def add(x: Double, y: Double): Double = x + y
  }

  def sum[A : NumberLike](xs: List[A]): A = xs match {
    case x :: rest => implicitly[NumberLike[A]].add(x, sum(rest))
    case Nil       => implicitly[NumberLike[A]].zero
  }
}
