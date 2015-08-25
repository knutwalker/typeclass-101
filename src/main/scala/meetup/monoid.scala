package meetup

import shapeless._

package object monoid {

  // add a bunch of things together


  trait Semigroup[A] {
    def add(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def zero: A
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

    implicit def derive[A, Repr](implicit
      gen: Generic.Aux[A, Repr],
      repr: Lazy[Monoid[Repr]])
    : Monoid[A] = new Monoid[A] {
      def zero = gen.from(repr.value.zero)
      def add(x: A, y: A) = gen.from(repr.value.add(gen.to(x), gen.to(y)))
    }

    implicit val proveHNil
    : Monoid[HNil] = new Monoid[HNil] {
      def zero = HNil
      def add(x: HNil, y: HNil) = HNil
    }

    implicit def proveHCons[H, T <: HList](implicit
      H: Lazy[Monoid[H]],
      T: Lazy[Monoid[T]])
    : Monoid[H :: T] = new Monoid[H :: T] {
      def zero = H.value.zero :: T.value.zero
      def add(x: H :: T, y: H :: T): H :: T =
        H.value.add(x.head, y.head) :: T.value.add(x.tail, y.tail)
    }
  }

  implicit class SemigroupOps[A](val x: A) extends AnyVal {
    def |+|(y: A)(implicit A: Semigroup[A]) = A.add(x, y)
  }

  import HList.ListCompat._
  def sum[A: Monoid](xs: List[A]): A = xs match {
    case x :: rest => x |+| sum(rest)
    case Nil       => Monoid[A].zero
  }
}
