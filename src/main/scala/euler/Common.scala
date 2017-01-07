package euler

import scala.collection.AbstractIterator

object Common {

  private def longIterator(start: Long) = new AbstractIterator[Long] {
    private var i = start
    def hasNext: Boolean = true
    def next(): Long = {
      val result = i
      i += 1
      result
    }
  }

  def compositesIterator() = new AbstractIterator[Long] {
    private val numbers = longIterator(2)
    private val primes = primesIterator()
    private var nextPrime = primes.next()

    override def hasNext: Boolean = true

    override def next(): Long = {
      var candidate = numbers.next()
      def isComposite = candidate < nextPrime
      while(!isComposite) {
        if(candidate == nextPrime) {
          nextPrime = primes.next()
          candidate = numbers.next()
        }
      }
      candidate
    }
  }

  def squaresIterator() = new AbstractIterator[Long] {
    var i = 0L
    override def hasNext: Boolean = true
    override def next(): Long = {
      i += 1
      i * i
    }
  }

  //from https://rosettacode.org/wiki/Sieve_of_Eratosthenes#Odds-Only_.22infinite.22_generator_sieve_using_Streams_and_Co-Inductive_Streams
  private object Primes {

    private class CIS[A](val start: A, val continue: () => CIS[A])

    private def merge(xs: CIS[Long], ys: CIS[Long]): CIS[Long] = {
      val (x, y) = (xs.start, ys.start)

      if (y > x) new CIS(x, () => merge(xs.continue(), ys))
      else if (x > y) new CIS(y, () => merge(xs, ys.continue()))
      else new CIS(x, () => merge(xs.continue(), ys.continue()))
    }

    private def primeMltpls(p: Long): CIS[Long] = {
      def nextCull(cull: Long): CIS[Long] = new CIS[Long](cull, () => nextCull(cull + 2 * p))

      nextCull(p * p)
    }

    private def allMltpls(ps: CIS[Long]): CIS[CIS[Long]] =
      new CIS[CIS[Long]](primeMltpls(ps.start), () => allMltpls(ps.continue()))

    private def join(ams: CIS[CIS[Long]]): CIS[Long] = {
      new CIS[Long](ams.start.start, () => merge(ams.start.continue(), join(ams.continue())))
    }

    private def oddPrimes(): CIS[Long] = {
      def oddPrms(n: Long, composites: CIS[Long]): CIS[Long] = {
        //"minua"
        if (n >= composites.start) oddPrms(n + 2, composites.continue())
        else new CIS[Long](n, () => oddPrms(n + 2, composites))
      }

      //following uses a new recursive source of odd base primes
      new CIS(3, () => oddPrms(5, join(allMltpls(oddPrimes()))))
    }

    def iterator(): Iterator[Long] = Iterator.single(2L) ++ Iterator.iterate(oddPrimes())(_.continue()).map(_.start)

  }

  def primesIterator(): Iterator[Long] = Primes.iterator()

}
