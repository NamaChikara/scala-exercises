
object NumberType extends Enumeration {
  val Deficient, Perfect, Abundant = Value
}

// which data structure to use?
// https://stackoverflow.com/questions/6928327/when-should-i-choose-vector-in-scala

object PerfectNumbers {

  // ERT https://alvinalexander.com/scala/fpbook/explaining-scala-val-function-syntax-functional-programming
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
        .map(x => x + 1)
        .reduceOption((a, b) => (a + 1) * (b + 1))
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


  def classify(input: Int): Either[String, NumberType.Value] = {
    if (input < 1) Left("Classification is only possible for natural numbers.")
    else {
      val aliquot = get_all_factors(input).filter(_ != input).sum
      aliquot match {
        case `input` => Right(NumberType.Perfect)
        case x if x > `input` => Right(NumberType.Abundant)
        case _ => Right(NumberType.Deficient)
      }
    }
  }

}


//def get_prime_factors(input: Int): Array[Int] = {
//
//  var g = Array[Int]()
//  var decomp = input
//
//  while (decomp % 2 == 0) {
//    g = g :+ 2
//    decomp = decomp / 2
//  }
//
//  var odd_i = 3
//  while (decomp <= input & decomp > 1) {
//    while (decomp % odd_i == 0) {
//      g = g :+ odd_i
//      decomp = decomp / odd_i
//    }
//    odd_i = odd_i + 2
//  }
//
//  g
//
//}

//def get_factors(input: Int): Set[Int] = {
//
//  val prime_factors = get_prime_factors(input)
//
//  var g = Array[Int](1)
//
//  for (i <- 1 until 1 << prime_factors.length) {
//    var factor = 1
//    for (j <- 0 until prime_factors.length) {
//      if ((i & (1 << j)) > 0) {
//        factor = factor * prime_factors(j)
//      }
//    }
//    g = g :+ factor
//  }
//
//  g.toSet
//
//}