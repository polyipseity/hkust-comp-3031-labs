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
  def nextLine(currentLine: List[Int]): List[Int] = ???

  /**
   * 3. Implement a lazy list `funSeq` which constructs this sequence.
   * Hint: use `LazyList.cons[A](a: A, b: => LazyList[A])`.
   */
  lazy val funSeq: LazyList[List[Int]] = ???

  // ------------------------------------------------------------------
  // Question 2
  // ------------------------------------------------------------------

  /**
   * 1. Write a lazy list of squares of integers ≥ 1.
   * You may use `LazyList.from(i: Int)`.
   */
  val squares: LazyList[Int] = ???

  /**
   * 2. Write a lazy list of all non‑empty strings using the characters
   * "0" and "1" and the concatenation operation +.
   *
   * In other words, every non‑empty string composed of "0" and "1"
   * should be reached at some point in the list.
   */
  lazy val codes: LazyList[String] = ???

  /**
   * 3. Using `codes`, write a lazy list of all possible non‑empty palindromes
   * of "0" and "1". You may use the `.reverse` function defined on strings.
   */
  val palCodes: LazyList[String] = ???

  /**
   * 4. Can you do the same without filtering?
   * The palindromes need not be in the same order.
   *
   * (Add your alternative implementation here.)
   */

  // ------------------------------------------------------------------
  // Question 5
  // ------------------------------------------------------------------

  /**
   * 5. Given another lazy list `otherCodes`, possibly finite or infinite,
   * you don't know at first:
   */
  val otherCodes: LazyList[String] = ???

  /**
   * Build a lazy list `allCodes` that interleaves `palCodes` and
   * `otherCodes`.
   */
  lazy val allCodes: LazyList[String] = ???
}
