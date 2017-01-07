package euler

import Common._

/**
  * https://projecteuler.net/problem=46
  * It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
  * 9 = 7 + 2×12
  * 15 = 7 + 2×22
  * 21 = 3 + 2×32
  * 25 = 7 + 2×32
  * 27 = 19 + 2×22
  * 33 = 31 + 2×12
  *
  * It turns out that the conjecture was false.
  *
  * What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
  */
object Euler46 extends App {

  //wrapping iterators in streams to memoize results for subsequent calls
  private lazy val primes = primesIterator().toStream
  private lazy val squares = squaresIterator().toStream

  private def oddComposites() = compositesIterator().filter(_ % 2 != 0)

  private def satisfiesConjecture(n: Long): Boolean = {
    val possibles = for {
      p <- primes.takeWhile(_ < n)
      s <- squares.takeWhile(_ * 2 < n)
    } yield p + (2 * s)
    possibles.contains(n)
  }

  def solve: Long = {
    oddComposites().find(!satisfiesConjecture(_)).getOrElse(-1)
  }

  println(s"Euler46 answer: $solve")

}
