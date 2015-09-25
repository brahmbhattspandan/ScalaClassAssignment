package edu.neu.coe.scala

import java.util.Random


trait RNG[+A] {
  def next: RNG[A]
  def value: A
}

object RNG {
  type SRNG[A] = Stream[RNG[A]]
  def rngs[A](r: RNG[A]): SRNG[A] = r #:: Stream.empty
  def values[A](s: SRNG[A]): Stream[A] = s map (_.value)
  def values2[A](s: SRNG[(A,A)]): Stream[A] = s flatMap(i => i.value._1 #:: i.value._2 #:: Stream.empty)      // Hint: use flatMap
}

abstract class RNG_Java[+A](n: Long) extends RNG[A] { 
  // must be overridden by sub-classes
  def value: A
  def newRNG(n: Long): RNG[A]
  // may be overridden (if you want to define your own pseudo-random sequence)
  def nextSeed: Long = RNG_Java.nextSeed(n)
  // base method -- not normally overridden
  def next: RNG[A] = newRNG(nextSeed)
  def state = n
}

object RNG_Java {
  def nextSeed(n: Long): Long = new Random(n).nextLong
}


case class LongRNG(n: Long) extends RNG_Java[Long](n) {
  def newRNG(n: Long): RNG[Long] = LongRNG(n)
  def value = n
}

object LongRNG {
  def apply: RNG[Long] = LongRNG(System.currentTimeMillis())
  def main(args: Array[String]): Unit = {
    val x = LongRNG(0L)
    println(x)
    println(x.next)
  }
}



case class DoubleRNG(n: Long) extends RNG_Java[Double](n) {
  def newRNG(n: Long) = DoubleRNG(n)
  def value = n.toDouble
  override def toString = s"DoubleRNG: $n->$value"

}


/**
 *  This is essentially a wrapper of Double where (implicitly) 0 <= x <= 1.
 *  Note that we would like to specify a require statement but such are not legal in Value types
 */
case class UniformDouble(x: Double) extends AnyVal {
  def + (y: Double) = x + y
}

object UniformDouble {
  def apply(x: Double, y: Unit) = if (x>=0 && x<=1) new UniformDouble(x) else throw new IllegalArgumentException(s"$x is not in range 0..1")
  def + (x: Double, y: UniformDouble) = y+x
}


/**
 * This class is a random-number-generator for values uniformly distributed in the range 0..1
 */
case class UniformDoubleRNG(n: Long) extends RNG_Java[UniformDouble](n) {
  def newRNG(n: Long) = UniformDoubleRNG(n)
  // the following, which calls the apply(Double,Unit) method will check that result is in range 0..1
  def value = UniformDouble(math.abs(n.toDouble/Long.MaxValue),Unit)
  override def toString = s"UniformDoubleRNG: $n->$value"
}

object UniformDoubleRNG {
  def apply: RNG[UniformDouble] = UniformDoubleRNG(System.currentTimeMillis())
}




/**
 * This class is a random-number-generator for values which are normally distributed with mean: 0 and standard deviation: 1
 * 
 * See <a href="https://en.wikipedia.org/wiki/Normal_distribution#Generating_values_from_normal_distribution">
 * Generating values from normal distribution (Box-Muller method)
 * </a>
 */
case class GaussianRNG(n: Long) extends RNG_Java[(Double,Double)](n) {
  def newRNG(n: Long) = GaussianRNG(n)
  val r1 = UniformDoubleRNG(n)
  val r2 = r1.next
  def value: (Double,Double) = {
    val u = r1.value.x
    val v = r2.value.x
    val k = if (u<=0) 0 else math.sqrt(-2*math.log(u))
    val l = if (v<=0) 0 else math.sqrt(-2*math.log(v))
    (k,l)
  }
  override def nextSeed: Long = RNG_Java.nextSeed(r2.asInstanceOf[RNG_Java[UniformDoubleRNG]].state)
  override def toString = s"GaussianRNG: $n->(${value._1},${value._2})"
}


object GaussianRNG {
  def apply: RNG[(Double,Double)] = GaussianRNG(System.currentTimeMillis())
}

