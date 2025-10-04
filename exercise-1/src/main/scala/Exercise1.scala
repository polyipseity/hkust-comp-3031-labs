// COMP3031Exercises.scala
//
// A single Scala 3 source that implements every exercise in the PDF.
// It contains all required functions, helpers, and a generic binary‑search tree.
//
// ────────────────────────────────────────────────────────────────

import scala.annotation.tailrec

object Exercise1 {
  /* ------------------------------------------------------------------
   * Part 1 – Functions as first‑class values
   * ------------------------------------------------------------------ */

  // Question 1
  def flip(f: (Int, Double) => Int): (Double, Int) => Int =
    (x, y) => f(y, x)

  // Question 2.1 – Identity for Int
  val id: Int => Int = x => x

  // Question 2.2 – Function composition
  def compose(f: Int => Int, g: Int => Int): Int => Int =
    x => f(g(x))

  // Question 2.3 – Repeated application of a function
  def repeated(f: Int => Int, n: Int): Int => Int = {
    @tailrec
    def rec(i: Int, acc: Int => Int): Int => Int =
      if (i == 0) acc
      else rec(i - 1, compose(f, acc))

    rec(n, id)
  }

  // Question 3.1 – Curry a two‑argument function
  def curry2(f: (Double, Int) => Boolean): Double => Int => Boolean =
    x => y => f(x, y)

  // Question 3.2 – Uncurry a curried function
  def uncurry2(f: Double => Int => Boolean): (Double, Int) => Boolean =
    (x, y) => f(x)(y)

  // Question 4 – Fixed point finder
  def fixedPoint(f: Int => Int): Int => Int = {
    @tailrec
    def rec(x: Int): Int = {
      val fx = f(x)
      if (fx == x) x else rec(fx)
    }

    rec
  }

  // Question 5.1 – Sum of a function over an interval
  def sum(f: Int => Int)(a: Int, b: Int): Int =
    @tailrec
    def loop(i: Int, acc: Int): Int =
      if (i > b) acc else loop(i + 1, acc + f(i))

    loop(a, 0)

  // Question 5.2 – Quadratic generator
  def quadratic(c: Int): Int => Int = x => (x - c) * (x - c)

  // Question 5.3 – Integrate (i‑3)² over an interval
  val quad3Integrate: (Int, Int) => Int =
    sum(quadratic(3))

  /* ------------------------------------------------------------------
   * Part 2 – Generic binary search tree
   * ------------------------------------------------------------------ */

  // Helper: equality from leq
  def eq[T](leq: (T, T) => Boolean)(x: T, y: T): Boolean =
    leq(x, y) && leq(y, x)

  // Helper: strict less‑than from leq
  def lt[T](leq: (T, T) => Boolean)(x: T, y: T): Boolean =
    !leq(y, x)

  // The generic tree ADT
  sealed abstract class Tree[T] {
    def size: Int

    def add(t: T): Tree[T]

    def toList: List[T]
  }

  case class EmptyTree[T](leq: (T, T) => Boolean) extends Tree[T] {
    override val size: Int = 0

    override def add(t: T): Tree[T] =
      Node(EmptyTree(leq), t, EmptyTree(leq), leq)

    override def toList: List[T] = Nil
  }

  case class Node[T](left: Tree[T], elem: T, right: Tree[T], leq: (T, T) => Boolean)
    extends Tree[T] {
    override val size: Int =
      left.size + 1 + right.size

    override def add(t: T): Tree[T] =
      if (!leq(elem, t)) Node(left.add(t), elem, right, leq)
      else if (!leq(t, elem)) Node(left, elem, right.add(t), leq)
      else this // duplicate – unchanged

    override def toList: List[T] =
      left.toList ++ (elem :: right.toList)
  }

  // Question 5 – Sorting a list with no duplicates
  def sortedList[T](leq: (T, T) => Boolean, ls: List[T]): List[T] = {
    val tree: Tree[T] = ls.foldLeft[Tree[T]](EmptyTree(leq))((t, e) => t.add(e))
    tree.toList
  }

  /* ------------------------------------------------------------------
   * Example usage
   * ------------------------------------------------------------------ */

  @main def demo(): Unit =
    println(s"flip: ${flip((i: Int, d: Double) => i + d.toInt)(3.14, 5)}")
    println(s"compose(id, _ + 1)(10): ${compose(id, (x: Int) => x + 1)(10)}")
    println(s"repeated(_ + 2, 3)(0): ${repeated((x: Int) => x + 2, 3)(0)}")
    println(s"curry2: ${curry2((d: Double, i: Int) => d < i)(1.5)(2)}")
    println(s"fixedPoint(_ / 2)(4): ${fixedPoint(x => x / 2)(4)}")
    println(s"sum(_ * _)(1,3): ${sum((x: Int) => x * x)(1, 3)}")
    println(s"quad3Integrate(1,5): ${quad3Integrate(1, 5)}")

    val intLeq = (x: Int, y: Int) => x <= y
    var tree: Tree[Int] = EmptyTree(intLeq)
    for (v <- List(2, 1, 3)) tree = tree.add(v)
    println(s"tree toList: ${tree.toList}")
    println(s"sortedList from [5, 3, 4]: ${sortedList(intLeq, List(5, 3, 4))}")
}
