package meetup

package calcops {

  object oop {

    trait Operand[A <: Operand[A]] {
      def add(y: A): A
      def sub(y: A): A
      def mult(y: A): A
    }

    // ====================

    case class Int32(x: Int) extends Operand[Int32] {
      def add(y: Int32): Int32 = Int32(x + y.x)
      def sub(y: Int32): Int32 = Int32(x - y.x)
      def mult(y: Int32): Int32 = Int32(x * y.x)
    }
    case class Int64(x: Long) extends Operand[Int64] {
      def add(y: Int64): Int64 = Int64(x + y.x)
      def sub(y: Int64): Int64 = Int64(x - y.x)
      def mult(y: Int64): Int64 = Int64(x * y.x)
    }
    case class Real32(x: Float) extends Operand[Real32] {
      def add(y: Real32): Real32 = Real32(x + y.x)
      def sub(y: Real32): Real32 = Real32(x - y.x)
      def mult(y: Real32): Real32 = Real32(x * y.x)
    }
  }

  object pattern {

    sealed trait Operand
    case class Int32(x: Int) extends Operand
    case class Int64(x: Long) extends Operand
    case class Real32(x: Float) extends Operand

    // ================

    def add[A <: Operand](left: A)(right: A): A =
      (left, right) match {
        case (Int32(x), Int32(y))   => Int32(x + y).asInstanceOf[A]
        case (Int64(x), Int64(y))   => Int64(x + y).asInstanceOf[A]
        case (Real32(x), Real32(y)) => Real32(x + y).asInstanceOf[A]
      }

    def sub[A <: Operand](left: A)(right: A): A =
      (left, right) match {
        case (Int32(x), Int32(y))   => Int32(x - y).asInstanceOf[A]
        case (Int64(x), Int64(y))   => Int64(x - y).asInstanceOf[A]
        case (Real32(x), Real32(y)) => Real32(x - y).asInstanceOf[A]
      }

    def mult[A <: Operand](left: A)(right: A): A =
      (left, right) match {
        case (Int32(x), Int32(y))   => Int32(x * y).asInstanceOf[A]
        case (Int64(x), Int64(y))   => Int64(x * y).asInstanceOf[A]
        case (Real32(x), Real32(y)) => Real32(x * y).asInstanceOf[A]
      }
  }

  object typeclass {

    trait CanAdd[A] {
      def add(x: A, y: A): A
    }

    // -------------

    trait CanMult[A] {
      def mult(x: A, y: A): A
    }

    // ==================
    // syntax

    implicit final class CanAddOps[A](val x: A) extends AnyVal {
      def add(y: A)(implicit A: CanAdd[A]): A =
        A.add(x, y)
    }

    // -------------

    implicit final class CanMultOps[A](val x: A) extends AnyVal {
      def mult(y: A)(implicit A: CanMult[A]): A =
        A.mult(x, y)
    }

    // =================

    sealed trait Operand
    case class Int32(x: Int) extends Operand
    case class Real32(x: Float) extends Operand

    // -----------------
    // instances

    implicit object Int32CanCalculate extends CanAdd[Int32] with CanMult[Int32] {
      def add(x: Int32, y: Int32) = Int32(x.x + y.x)
      def mult(x: Int32, y: Int32) = Int32(x.x * y.x)
    }

    // -------------

    implicit object Real32CanCalculate extends CanAdd[Real32] with CanMult[Real32] {
      def add(x: Real32, y: Real32) = Real32(x.x + y.x)
      def mult(x: Real32, y: Real32) = Real32(x.x * y.x)
    }

    // ==================

    def add[A: CanAdd](left: A, right: A) =
      left add right

    // -------------

    def mult[A: CanMult](left: A, right: A) =
      left mult right
  }
}
