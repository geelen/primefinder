import Math.{ceil,sqrt}

object PrimeFinder {
  val maxVal = 100000
  private val primes : Array[Boolean] = {
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

  def isPrime(num : Int) : Boolean = {
    if (num > maxVal) throw new IllegalArgumentException
    primes(num - 1)
  }
}

object Main {
  def main(args : Array[String]) {
    println("main")
    0.until(10).foreach(a => {
      println(0.until(10).map(b => {
        0.until(10).map(c => {
          PrimeFinder.isPrime(a*100 + b*10 + c + 1).toString()(0).toUpperCase
        }).foldLeft("")(_+_)
      }).foldLeft("")(_+ " " +_))
    })
    //f(x) = x^2 - x + 41
  }
}