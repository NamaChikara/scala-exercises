
object PerfectNumbers {

  private def get_prime_factors(input: Int): Array[Int] = {

    var g = Array[Int]()
    var decomp = input

    while (decomp % 2 == 0) {
      g = g :+ 2
      decomp = decomp / 2
    }

    var odd_i = 3
    while (decomp <= input & decomp > 1) {
      while (decomp % odd_i == 0) {
        g = g :+ odd_i
        decomp = decomp / odd_i
      }
      odd_i = odd_i + 2
    }

    g

  }

  private def get_factors(input: Int): Array[Int] = {

    val prime_factors = get_prime_factors(input)

    var g = Array[Int]()

    for (i <- 1 until 2 ^ g.length) {

    }

  }

  def classify(input: Int): String = {



    "A"
  }

}