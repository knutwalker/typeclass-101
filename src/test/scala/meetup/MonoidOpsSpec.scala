package meetup
package monoid

import org.scalatest._

class MonoidOpsSpec extends FlatSpec with Matchers {

  behavior of "sum"

  it should "sum a List of ints" in {

    sum(List(1, 2, 3, 4, 5)) should be (15)
  }
}
