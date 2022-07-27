import cats.Order
import cats.collections.PairingHeap
import cats.instances.long.*
import PairingHeap.empty
import scala.annotation.tailrec

type Longs = LazyList[Long]

def spin(start: Long, wheel: Longs): Longs =
  def cycle(ns: Longs): Longs = ns.lazyAppendedAll(cycle(ns))
  cycle(wheel).scan(start)(_ + _)

def toComposites(ns: Longs): Longs =
  def loop(m: Long, ms: Longs): Longs = (m * ms.head) #:: loop(m, ms.tail)
  loop(ns.head, ns)

def sieve(candidates: Longs, composites: PairingHeap[Longs]): Longs =
  val n #:: ns = candidates 
  composites.minimumOption.fold {
    n #:: sieve(ns, PairingHeap(toComposites(candidates)))
  } { case m #:: ms =>
      if   n < m
      then n #:: sieve(ns, composites + toComposites(candidates))
      else sieve(if m == n then ns else candidates, composites.remove + ms)
  }

def primes: Longs =
  val startingPrimes = LazyList(2L, 3L, 5L)
  val wheel          = LazyList(4L, 2L, 4L, 2L, 4L, 6L, 2L, 6L)
  sieve(startingPrimes.lazyAppendedAll(spin(7, wheel)), empty)

val criteria = List(
  2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73,
  79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163,
  167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251,
  257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
  353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443,
  449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541)
assert(criteria == primes.take(100).toList)
// primes.drop(999999).head // : Long = 154863
1
