package meetup
package foldable

import org.scalatest._

class FoldableOpsSpec extends FlatSpec with Matchers {

  behavior of "sum"

  it should "sum a List of ints" in {

    sum(List(1, 2, 3, 4, 5)) should be (15)
  }

  it should "sum a Vector of ints" in {

    sum(Vector(1, 2, 3, 4, 5)) should be (15)
  }
}
