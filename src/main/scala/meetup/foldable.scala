package meetup

package object foldable {
  import monoid.Monoid
  import monoid.SemigroupOps


  def sum[A: Monoid](xs: List[A]): A = xs match {
    case x :: rest => x |+| sum(rest)
    case Nil       => Monoid[A].zero
  }
}
