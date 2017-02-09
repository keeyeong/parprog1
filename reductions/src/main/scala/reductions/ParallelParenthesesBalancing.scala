package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    def balanceIter(bal: Int, chars: Array[Char]): Int = {
      if (chars.isEmpty) bal else {
        if (chars.head == ')' && bal <= 0)
        // if we ever encounter a ) without any preceeding ( it is already unbalanced, skip the rest
          balanceIter(-1, Array[Char]())
        else if (chars.head == '(')
          balanceIter(bal + 1, chars.tail)
        else if (chars.head == ')')
          balanceIter(bal - 1, chars.tail)
        else
          balanceIter(bal, chars.tail)
      }
    }

    balanceIter(0, chars) == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    def traverse(idx: Int, until: Int, unmatchedLeft: Int, unmatchedRight: Int): (Int, Int) = {
      if (idx == until) {
        (unmatchedLeft, unmatchedRight)
      }
      else {
        if (chars(idx) == '(') {
          traverse(idx + 1, until, unmatchedLeft + 1, unmatchedRight)
        } else if (chars(idx) == ')') {
          if (unmatchedLeft > 0)
            traverse(idx + 1, until, unmatchedLeft - 1, unmatchedRight)
          else
            traverse(idx + 1, until, unmatchedLeft, unmatchedRight + 1)
        } else {
          traverse(idx + 1, until, unmatchedLeft, unmatchedRight)
        }
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val mid = from + ((until - from) / 2)
      if (threshold > 0 && until - from > threshold) {
        val (resLeft, resRight) = parallel(reduce(from, mid), reduce(mid, until))
        // result from left can be balanced with result from right if left has ( and right has )
        // otherwise add them up for the next combine
        if (resLeft._1 > 0 && resRight._2 > 0) {
          if (resLeft._1 - resRight._2 > 0) (resLeft._1 - resRight._2, resLeft._2) else (resRight._1, resRight._2 - resLeft._1)
        } else {
          (resLeft._1 + resRight._1, resLeft._2 + resRight._2)
        }
      } else {
        traverse(from, until, 0, 0)
      }
    }

    reduce(0, chars.length) == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
