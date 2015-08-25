package meetup
package foldable

import language.higherKinds

import org.scalatest._

class FoldableOpsSpec extends FlatSpec with Matchers {

  behavior of "sum"

  it should "sum a List of ints" in {

    sum(List(1, 2, 3, 4, 5)) should be (15)
  }

  it should "sum a Vector of ints" in {

    sum(Vector(1, 2, 3, 4, 5)) should be (15)
  }

  it should "sum a one and of Points" in {
    case class Point(x: Int, y: Int)

    type NonEmptyList[A] = OneAnd[List, A]

    val points: NonEmptyList[Point] =
      OneAnd(Point(1, 2), List(Point(3, 4), Point(5, 6)))

    sum[NonEmptyList, Point](points) should be (Point(9, 12))
  }
}


case class OneAnd[F[_], A](head: A, tail: F[A])
object OneAnd {
  implicit def oneAndFold[F[_]: Fold]: Fold[({type l[a] = OneAnd[F, a]})#l] =
    new OneAndFold[F] {
      def F = Fold[F]
    }

}

sealed trait OneAndFold[F[_]] extends Fold[({type l[a] = OneAnd[F, a]})#l] {
  def F: Fold[F]

  def foldl[A, B](fa: OneAnd[F, A])(zero: B)(f: (B, A) => B): B =
    F.foldl(fa.tail)(f(zero, fa.head))(f)
}
