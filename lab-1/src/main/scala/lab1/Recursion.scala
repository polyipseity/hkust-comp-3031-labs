package lab1

import scala.annotation.tailrec

object Recursion:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    @tailrec
    def impl(cache: Map[(Int, Int), Int], path: List[(Int, Int)]): Map[(Int, Int), Int] = path match {
      case (c, r) :: next => (c, r) match {
        case (c, r) if r < 0 || c < 0 || c > r => impl(cache.updated((c, r), 0), next)
        case (c, r) if c == 0 || c == r => impl(cache.updated((c, r), 1), next)
        case _ => (cache.get((c - 1, r - 1)), cache.get(c, r - 1)) match {
          case (None, _) => impl(cache, (c - 1, r - 1) :: path)
          case (_, None) => impl(cache, (c, r - 1) :: path)
          case (Some(left), Some(right)) => impl(cache.updated((c, r), left + right), next)
        }
      }
      case Nil => cache
    }

    impl(Map.empty, (c, r) :: Nil)(c, r)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    @tailrec
    def impl(level: Int, chars: List[Char]): Boolean = if level < 0
    then false
    else chars match {
      case head :: next => impl(level + (head match {
        case '(' => 1
        case ')' => -1
        case _ => 0
      }), next)
      case Nil => level == 0
    }

    impl(0, chars)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    val coinsMap = coins.groupMapReduce(identity)(Function.const(1))(_ + _)
      .view.mapValues(Function.const(money / coins.min)).toMap // `coins` are assumed unlimited in tests

    @tailrec
    def impl(
              cache: Map[(Int, Map[Int, Int]), Int],
              path: List[(Int, Map[Int, Int])],
            ): Map[(Int, Map[Int, Int]), Int] = path match {
      case (money, coins) :: next => money match {
        case money if money < 0 => impl(cache.updated((money, coins), 0), next)
        case money if money == 0 => impl(cache.updated((money, coins), 1), next)
        case _ =>
          val children = coins.headOption.map((coin, count) =>
            (0 to count).view.map(money - coin * _).map((_, coins.removed(coin)))
              .map((money, coins) => (money, coins, cache.get(money, coins)))
          ).getOrElse(Nil)
          children.find(_._3.isEmpty) match {
            case Some((childMoney, childCoins, _)) => impl(cache, (childMoney, childCoins) :: path)
            case None => impl(cache.updated((money, coins), children.map(_._3.get).sum), next)
          }
      }
      case Nil => cache
    }

    impl(Map.empty, (money, coinsMap) :: Nil)(money, coinsMap)
