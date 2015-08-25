package meetup

package object monoid {

  // add a bunch of things together


  def sum(xs: List[Int]): Int = xs match {
    case x :: rest => x + sum(rest)
    case Nil       => 0
  }
}
