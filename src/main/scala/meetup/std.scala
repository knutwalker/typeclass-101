package meetup

package object std {

  trait Equality[A] {
    def eql(x: A, y: A): Boolean

    def neq(x: A, y: A): Boolean =
      !eql(x, y)
  }

  implicit class EqualityOps[A](val x: A) extends AnyVal {
    def ===(y: A)(implicit A: Equality[A]): Boolean =
      A.eql(x, y)

    def ≠(y: A)(implicit A: Equality[A]): Boolean =
      A.neq(x, y)
  }
}
