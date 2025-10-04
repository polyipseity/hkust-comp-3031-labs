package lab1

class RecursionSuite extends munit.FunSuite:

  import Recursion.*

  // ------ balance tests -----------------------------------------------------

  test("balance: '(if (zero? x) max (/ 1 x))' is balanced") {
    assert(balance("(if (zero? x) max (/ 1 x))".toList))
  }

  test("balance: 'I told him ...' is balanced") {
    assert(balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList))
  }

  test("balance: ':-)' is unbalanced") {
    assert(!balance(":-)".toList))
  }

  test("balance: counting is not enough") {
    assert(!balance("())(".toList))
  }

  // ------ countChange tests -------------------------------------------------

  test("countChange: example given in the guide") {
    assertEquals(countChange(4, List(1, 2)), 3)
  }

  test("countChange: sorted HKD") {
    assertEquals(countChange(300, List(5, 10, 20, 50, 100, 200, 500)), 1022)
  }

  test("countChange: no pennies") {
    assertEquals(countChange(301, List(5, 10, 20, 50, 100, 200, 500)), 0)
  }

  test("countChange: unsorted HKD") {
    assertEquals(countChange(300, List(500, 5, 50, 100, 20, 200, 10)), 1022)
  }

  // ------ pascal tests ------------------------------------------------------

  test("pascal: col=0,row=2") {
    assertEquals(pascal(0, 2), 1)
  }

  test("pascal: col=1,row=2") {
    assertEquals(pascal(1, 2), 2)
  }

  test("pascal: col=1,row=3") {
    assertEquals(pascal(1, 3), 3)
  }

  test("pascal: col=3,row=8") {
    assertEquals(pascal(3, 8), 56)
  }

  import scala.concurrent.duration.*

  override val munitTimeout: Duration = 10.seconds
