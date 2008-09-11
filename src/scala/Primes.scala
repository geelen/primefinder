import Math.{ceil,sqrt}

object PrimeArray {
  def getPrimes(maxVal : Int) = {
    val startTime = System.nanoTime
    println("primes init")
    val primes : Array[Boolean] = Array.make(maxVal, true)
    primes(0) = true
    2.until(ceil(sqrt(maxVal)).toInt + 1).foreach((a : Int) => {
      if (primes(a - 1)) {
        (a*2).until(maxVal+1,a).foreach((b : Int) => {primes(b-1) = false})
      }
    })
    primes(1) = true
    println("took " + ((System.nanoTime - startTime) / 1000000000L.toDouble) + " seconds.")
    primes
  }
}

object PrimeFinder {
  private val maxVal = 100000000
  private val primes = PrimeArray.getPrimes(maxVal)

  def isPrime(num : Int) : Boolean = {
    if (num > maxVal) throw new IllegalArgumentException
    primes(num - 1)
  }
}

object Main {
  def main(args : Array[String]) {
    println("main")
    val f = (x : Int) => { x*x - x + 41 }
    0.until(10000,1000).foreach((x : Int) => {
      val fx = f(x)
      val primeness = PrimeFinder.isPrime(fx)
      println("f(" + x + ") = " + fx + " is prime: " + primeness)
    })
  }
}