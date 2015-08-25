package meetup

package calcops {

  object oop {

    trait Operand[A <: Operand[A]] {
      def add(y: A): A
      def mult(y: A): A
    }

    // ====================

    case class Int32(x: Int) extends Operand[Int32] {
      def add(y: Int32): Int32 = Int32(x + y.x)
      def mult(y: Int32): Int32 = Int32(x * y.x)
    }
    case class Real32(x: Float) extends Operand[Real32] {
      def add(y: Real32): Real32 = Real32(x + y.x)
      def mult(y: Real32): Real32 = Real32(x * y.x)
    }
  }

  object pattern {

    sealed trait Operand
    case class Int32(x: Int) extends Operand
    case class Real32(x: Float) extends Operand

    // ================

    def add[A <: Operand](left: A)(right: A): A =
      (left, right) match {
        case (Int32(x), Int32(y))   => Int32(x + y).asInstanceOf[A]
        case (Real32(x), Real32(y)) => Real32(x + y).asInstanceOf[A]
      }

    def mult[A <: Operand](left: A)(right: A): A =
      (left, right) match {
        case (Int32(x), Int32(y))   => Int32(x * y).asInstanceOf[A]
        case (Real32(x), Real32(y)) => Real32(x * y).asInstanceOf[A]
      }
  }
}
