package meetup

import language.higherKinds

package object foldable {
  import monoid.Monoid

  trait Fold[F[_]] {
    def foldl[A, B](fa: F[A])(zero: B)(f: (B, A) => B): B
  }
  object Fold {
    def apply[F[_]](implicit F: Fold[F]): Fold[F] = F

    implicit object ListFold extends Fold[List] {
      def foldl[A, B](fa: List[A])(zero: B)(f: (B, A) => B): B =
        fa.foldLeft(zero)(f)
    }

    implicit object VectorFold extends Fold[Vector] {
      def foldl[A, B](fa: Vector[A])(zero: B)(f: (B, A) => B): B =
        fa.foldLeft(zero)(f)
    }
  }
  implicit class FoldOps[F[_], A](val self: F[A]) extends AnyVal {
    def foldl[B](zero: B)(f: (B, A) => B)(implicit F: Fold[F]): B =
      F.foldl(self)(zero)(f)

    def sum(implicit F: Fold[F], A: Monoid[A]): A =
      F.foldl(self)(A.zero)(A.add)
  }


  def sum[F[_]: Fold, A: Monoid](xs: F[A]): A = xs.sum
}
