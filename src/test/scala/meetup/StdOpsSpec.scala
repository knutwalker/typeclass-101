package meetup
package std

import org.scalatest._

class StdOpsSpec extends FlatSpec with Matchers with NonImplicitAssertions {

  behavior of "Equality"

  it should "provide type-safe equality" in {

    ("foo" === "foo") should be(true)
    ("foo" === "bar") should be(false)
    """("foo" === 42)""" shouldNot typeCheck
  }

  behavior of "Comparable"

  it should "provide type-safe comparing" in {

    ("foo" ≤ "foo") should be(true)
    ("foo" ≤ "bar") should be(false)
    """("foo" ≤ 42)""" shouldNot typeCheck
  }
}
