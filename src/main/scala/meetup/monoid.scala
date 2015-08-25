package meetup

package object monoid {

  // add a bunch of things together


  // fails: no supertype, looses typeinfo, how to get 0
  // def sum(xs: List[Number]): Number = xs match {
  //   case x :: rest => x + sum(rest)
  //   case Nil       => ???
  // }
}
