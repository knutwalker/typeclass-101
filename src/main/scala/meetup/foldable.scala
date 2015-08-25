package meetup

package object foldable {
  import monoid.Monoid


  def sum[A: Monoid](xs: Traversable[A]): A =
    xs.foldLeft(Monoid[A].zero)(Monoid[A].add)
}
