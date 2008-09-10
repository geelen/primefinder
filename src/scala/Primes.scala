object PrimeFinder {
  def isPrime(num : Int) : Boolean = {
    2.until(num / 2).find((a : Int) => {
      val div : Double = (num.toDouble / a.toDouble)
      div - div.toInt == 0.0
    }).isEmpty
  }
}

object Main {
  def main(args : Array[String]) {
    test(2.until(10))
  }

  def test(nums : Iterable[Int]) {
    nums.foreach((a : Int) => println(a + " is prime: " + PrimeFinder.isPrime(a)))
  }
}