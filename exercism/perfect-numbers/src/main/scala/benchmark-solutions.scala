def aliquot(n:Int):Int =
  (1 to n/2).filter(n % _==0 ).sum

val next_odd_number: Int => Int = (x) => (x + 1) / 2 * 2 + 1

def get_prime_factors(value: Int, factor: Int = 2,
                      all_factors: List[Int] = List[Int]()): List[Int] = {

  if (value == 1) {
    all_factors
  } else if (value % factor == 0) {
    // divide the factor out of value, add it to the array of factors, and repeat
    get_prime_factors(value / factor, factor, factor :: all_factors)
  } else {
    // check the next odd number (non-prime odd numbers will never be factors of the
    // current value since all their prime factors have already been divided out)
    get_prime_factors(value, next_odd_number(factor), all_factors)
  }

}

def get_all_factors(input: Int): Set[Int] = {

  val prime_factors = get_prime_factors(input)

  val all_factors = for (i <- 0 until 1 << prime_factors.length) yield {
    val subset = for (j <- 0 until prime_factors.length) yield {
      if ((i & (1 << j)) > 0) prime_factors(j) else 1
    }
    // if prime_factors is empty, subset will be empty. 1 is a factor of every integer
    subset.reduceOption(_ * _).getOrElse(1)
  }

  all_factors.toSet
}

def aliquot_mine(input: Int): Int =
  get_all_factors(input).filter(_ != input).sum



def get_prime_f(value: Int, factor: Int = 2,
                all_factors: Map[Int, Int] = Map[Int, Int]().withDefaultValue(0)):
Map[Int, Int] = {

  if (value == 1) {
    all_factors
  } else if (value % factor == 0) {
    // divide the factor out of value, add it to the array of factors, and repeat
    get_prime_f(value / factor, factor, all_factors + (factor -> (all_factors(factor) + 1)))
  } else {
    // check the next odd number (non-prime odd numbers will never be factors of the
    // current value since all their prime factors have already been divided out)
    get_prime_f(value, next_odd_number(factor), all_factors)
  }

}

def get_all_f(input: Int): Set[Int] = {

  val prime_factors = get_prime_f(input)

  val repeat_count = prime_factors.keys.map(
    x => x -> prime_factors
      .view
      .filterKeys(_ > x)
      .values
      .map(_ + 1)
      .reduceOption((a, b) => a * b)
      .getOrElse(1)
  ).toMap

  val num_factors = prime_factors.values.map(_ + 1).reduce(_ * _)

  val all_factors = for (i <- 0 until num_factors) yield {

    prime_factors.keys.map(x =>
      scala.math.pow(x.toDouble, prime_factors(x) - (i / repeat_count(x)) % (prime_factors(x) + 1))
    ).reduce(_ * _).toInt

  }

  all_factors.toSet
}

def aliquot_f(input: Int): Int =
  get_all_f(input).filter(_ != input).sum


val r = new scala.util.Random(100)

val numbers = for (i <- 1 to 50) yield r.nextInt(100000000)

val elapsed_2 = for (i <- 1 to 5) yield {
  val t0 = System.nanoTime()
  numbers.foreach(x => aliquot_f(x))
  val t1 = System.nanoTime()
  1e-9 * (t1 - t0)
}

val elapsed_23 = for (i <- 1 to 1) yield {

  numbers.map(x => aliquot(x))

}