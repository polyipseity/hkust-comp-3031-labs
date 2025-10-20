import scala.annotation.tailrec
// Exercise3.scala
// ------------------------------------------------------------
//  Exercise Session 3 : For comprehensions and lazy lists
// ------------------------------------------------------------

object Exercise3 {

  // ------------------------------------------------------------------
  // Question 1
  // ------------------------------------------------------------------

  /**
   * 1. Find the next element in the following sequence:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * (No code required – just think about it!)
   */

  /**
   * 2. Write a function to compute the next line of the sequence.
   */
  def nextLine(currentLine: List[Int]): List[Int] =
    @tailrec
    def impl(ret: List[Int], currentLine: List[Int]): List[Int] =
      currentLine match {
        case head :: next =>
          val (same, diff) = currentLine.span(_ == head)
          impl(head :: same.size :: ret, diff)
        case Nil => ret.reverse
      }

    impl(Nil, currentLine)

  /**
   * 3. Implement a lazy list `funSeq` which constructs this sequence.
   * Hint: use `LazyList.cons[A](a: A, b: => LazyList[A])`.
   */
  lazy val funSeq: LazyList[List[Int]] =
    var cur = List(1)

    def impl: LazyList[List[Int]] = LazyList.cons({
      val ret = cur;
      cur = nextLine(cur);
      ret
    }, impl)

    impl

  // ------------------------------------------------------------------
  // Question 2
  // ------------------------------------------------------------------

  /**
   * 1. Write a lazy list of squares of integers ≥1.
   * You may use `LazyList.from(i: Int)`.
   */
  val squares: LazyList[Int] =
    // LazyList.from(1).map(x => x * x)
    lazy val xs: LazyList[Int] = 1 #:: xs.map(_ + 1)
    xs.map(x => x * x)

  /**
   * 2. Write a lazy list of all non‑empty strings using the characters
   * "0" and "1" and the concatenation operation +.
   *
   * In other words, every non‑empty string composed of "0" and "1"
   * should be reached at some point in the list.
   */
  lazy val codes: LazyList[String] =
    var cur = List("0", "1")

    def codesGroup: LazyList[List[String]] = LazyList.cons((cur, cur = cur.map("0" + _) ::: cur.map("1" + _))._1, codesGroup)

    codesGroup.flatten

  /**
   * 3. Using `codes`, write a lazy list of all possible non‑empty palindromes
   * of "0" and "1". You may use the `.reverse` function defined on strings.
   */
  val palCodes: LazyList[String] =
    codes.filter(x => x.reverse == x)

  /**
   * 4. Can you do the same without filtering?
   * The palindromes need not be in the same order.
   *
   * (Add your alternative implementation here.)
   */
  // val palCodes: LazyList[String] =
  //   codes.filter(x => x.reverse == x)

  // ------------------------------------------------------------------
  // Question 5
  // ------------------------------------------------------------------

  /**
   * 5. Given another lazy list `otherCodes`, possibly finite or infinite,
   * you don't know at first:
   */
  val otherCodes: LazyList[String] = LazyList("A", "B")

  /**
   * Build a lazy list `allCodes` that interleaves `palCodes` and
   * `otherCodes`.
   */
  lazy val allCodes: LazyList[String] =
    LazyList.from(0).map(idx => {
      (palCodes.drop(idx), otherCodes.drop(idx)) match {
        case (head #:: next, head2 #:: next2) => List(head, head2)
        case (head #:: _, _) => List(head)
        case (_, head #:: _) => List(head)
        case _ => Nil
      }
    }).takeWhile(_.nonEmpty).flatten

  @main
  def main(): Unit =
    // ------------------------------------------------------------------
    // Question 1 – nextLine
    // ------------------------------------------------------------------

    assert(nextLine(List(1)) == List(1, 1))
    assert(nextLine(List(1, 1)) == List(2, 1))
    assert(nextLine(List(2, 1)) == List(1, 2, 1, 1))
    assert(nextLine(List(1, 2, 1, 1)) == List(1, 1, 1, 2, 2, 1))

    // ------------------------------------------------------------------
    // Question 1 – funSeq
    // ------------------------------------------------------------------

    val firstFiveFunSeq = funSeq.take(5).toList
    println(firstFiveFunSeq)
    assert(firstFiveFunSeq.head == List(1))
    assert(firstFiveFunSeq(1) == List(1, 1))
    assert(firstFiveFunSeq(2) == List(2, 1))
    assert(firstFiveFunSeq(3) == List(1, 2, 1, 1))
    assert(firstFiveFunSeq(4) == List(1, 1, 1, 2, 2, 1))

    // ------------------------------------------------------------------
    // Question 2 – squares
    // ------------------------------------------------------------------

    val firstTenSquares = squares.take(10).toList
    println(firstTenSquares)
    assert(firstTenSquares == List(1, 4, 9, 16, 25, 36, 49, 64, 81, 100))

    // ------------------------------------------------------------------
    // Question 2 – codes (non‑empty binary strings)
    // ------------------------------------------------------------------

    val firstEightCodes = codes.take(8).toList
    println(firstEightCodes)
    assert(firstEightCodes == List("0", "1", "00", "01", "10", "11", "000", "001"))

    // Check that all returned strings consist only of '0' and '1'
    assert(firstEightCodes.forall(_.forall(ch => ch == '0' || ch == '1')))

    // ------------------------------------------------------------------
    // Question 2 – palCodes (palindromes)
    // ------------------------------------------------------------------

    val firstSixPalCodes = palCodes.take(6).toList
    println(firstSixPalCodes)
    assert(firstSixPalCodes == List("0", "1", "00", "11", "000", "010"))

    // Verify palindrome property
    assert(firstSixPalCodes.forall(s => s == s.reverse))

    // ------------------------------------------------------------------
    // Question 5 – allCodes (interleaving)
    // ------------------------------------------------------------------

    // The first four elements should be:
    //   palCodes(0), otherCodes(0), palCodes(1), otherCodes(1)
    val firstFourAllCodes = allCodes.take(4).toList
    println(firstFourAllCodes)
    assert(firstFourAllCodes == List("0", "A", "1", "B"))

    val firstEightAllCodes = allCodes.take(8).toList // Intentionally take more...
    println(firstEightAllCodes)
    assert(firstEightAllCodes == List("0", "A", "1", "B", "00", "11", "000", "010"))

    println("All tests passed!")
}
