package meetup

package object monoid {

  // add a bunch of things together


  trait Monoid[A] {
    def zero: A
    def add(x: A, y: A): A
  }
  object Monoid {
    def apply[A](implicit A: Monoid[A]): Monoid[A] = A

    implicit object IntNumber extends Monoid[Int] {
      def zero: Int = 0
      def add(x: Int, y: Int): Int = x + y
    }
    implicit object DoubleNumber extends Monoid[Double] {
      def zero: Double = 0.0
      def add(x: Double, y: Double): Double = x + y
    }
  }

  implicit class MonoidOps[A](val x: A) extends AnyVal {
    def |+|(y: A)(implicit A: Monoid[A]) = A.add(x, y)
  }

  def sum[A: Monoid](xs: List[A]): A = xs match {
    case x :: rest => x |+| sum(rest)
    case Nil       => Monoid[A].zero
  }
}
