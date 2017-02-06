package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
    * coins for the specified amount of money.
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) 0
      else if (money == coins.head) 1
      else {
        val (a, b) = (countChangeIter(money - coins.head, coins), countChangeIter(money, coins.tail))
        a + b
      }
    }

    if (money == 0) 1
    else
      countChangeIter(money, coins)
  }

  type Threshold = (Int, List[Int]) => Boolean

  /** In parallel, counts the number of ways change can be made from the
    * specified list of coins for the specified amount of money.
    */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    def parCountChangeIter(money: Int, coins: List[Int]): Int = {
      if (coins.isEmpty || money < 0) 0
      else if (money == coins.head) 1
      else {
        if (threshold(money,coins)) countChange(money, coins)
        else {
          val (a, b) = parallel(parCountChangeIter(money - coins.head, coins), parCountChangeIter(money, coins.tail))
          a + b
        }
      }
    }

    if (money == 0) 1
    else
      parCountChangeIter(money, coins)
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold =
    (amountLeft: Int, coinsLeft: List[Int]) => amountLeft <= 2 * startingMoney / 3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (amountLeft: Int, coinsLeft: List[Int]) => coinsLeft.size <= 2 * totalCoins / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold = {
    (amountLeft: Int, coinsLeft: List[Int]) => coinsLeft.size * amountLeft <= 2 * allCoins.size * startingMoney / 3
  }
}
