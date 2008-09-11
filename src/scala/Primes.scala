import Math.{ceil,sqrt}

object PrimeArray {
  def getPrimes(maxVal : Int) = {
    val primes : Array[Boolean] = Array.make(maxVal, true)
    2.until(ceil(sqrt(maxVal)).toInt + 1).foreach((a : Int) => {
      if (primes(a - 1)) {
        (a*2).until(maxVal+1,a).foreach((b : Int) => {primes(b-1) = false})
      }
    })
    primes
  }
}

object PrimeFinder {

  def isPrime(primes : Array[Boolean], num : Int) : Boolean = {
    if (num > primes.length) throw new IllegalArgumentException
    primes(num - 1)
  }
}

object Main {
  private val maxVal = 100000000

  def main(args : Array[String]) {
    println("Starting main for maxVal " + maxVal)
    val primesArray = time("Building prime array", {Unit => PrimeArray.getPrimes(maxVal)})
    val numPrimes = time("Counting primes", {Unit => 1.until(maxVal).filter(PrimeFinder.isPrime(primesArray,_)).length})
    println("Found " + numPrimes + " primes")
  }

  def time[B](message : String, f : Unit => B) = {
    val startTime = System.nanoTime
    val result : B = f()
    println(message + " took " + ((System.nanoTime - startTime) / 1000000000L.toDouble) + " seconds.")
    result
  }
}