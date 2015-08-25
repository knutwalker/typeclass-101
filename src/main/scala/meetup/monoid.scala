package meetup

package object monoid {

  // add a bunch of things together


  def sum(xs: List[Int]): Int = xs match {
    case x :: rest => x + sum(rest)
    case Nil       => 0
  }

  // fails: WET code, generally also type erasure issues
  def sum(xs: List[Double]): Double = xs match {
    case x :: rest => x + sum(rest)
    case Nil       => 0.0d
  }
}
