package meetup

package object std {

  trait Equality[A] {
    def eql(x: A, y: A): Boolean

    def neq(x: A, y: A): Boolean =
      !eql(x, y)
  }
  object Equality {
    implicit def naturalEquality[A]: Equality[A] = new Equality[A] {
      def eql(x: A, y: A): Boolean = x == y
    }
  }

  trait Comparable[A] extends Equality[A] {
    def lte(x: A, y: A): Boolean

    def lt(x: A, y: A): Boolean =
      lte(x, y) && !lte(y, x)

    def gte(x: A, y: A): Boolean =
      !lte(x, y) || lte(y, x)

    def gt(x: A, y: A): Boolean =
      !lte(x, y)

    def eql(x: A, y: A): Boolean =
      lte(x, y) && lte(y, x)
  }
  object Comparable {
    implicit def byScalaOrd[A](implicit A: Ordering[A]): Comparable[A] = new Comparable[A] {
      def lte(x: A, y: A): Boolean =
        A.compare(x, y) <= 0
    }
  }

  implicit class EqualityOps[A](val x: A) extends AnyVal {
    def ≡(y: A)(implicit A: Equality[A]): Boolean =
      A.eql(x, y)

    def ≠(y: A)(implicit A: Equality[A]): Boolean =
      A.neq(x, y)
  }

  implicit class ComparableOps[A](val x: A) extends AnyVal {
    def ≤(y: A)(implicit A: Comparable[A]): Boolean =
      A.lte(x, y)

    def <(y: A)(implicit A: Comparable[A]): Boolean =
      A.lt(x, y)

    def ≥(y: A)(implicit A: Comparable[A]): Boolean =
      A.gte(x, y)

    def >(y: A)(implicit A: Comparable[A]): Boolean =
      A.gt(x, y)
  }
}
