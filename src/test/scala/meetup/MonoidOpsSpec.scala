package meetup
package monoid

import org.scalatest._

class MonoidOpsSpec extends FlatSpec with Matchers {

  behavior of "sum"

  it should "sum a List of ints" in {

    sum(List(1, 2, 3, 4, 5)) should be (15)
  }

  it should "sum a List of doubles" in {

    sum(List(1.0, 2.0, 3.0, 4.0, 5.0)) should be (15.0)
  }
}
