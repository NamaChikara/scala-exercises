object SumOfMultiples {

  def sum(factors: Set[Int], limit: Int): Int = {

    val multiples = for (f <- factors) yield {
      Iterator
        .iterate(f){ _ + f }
        .takeWhile(_ < limit)
        .toSet
    }

    multiples.flatten.sum

  }
}

//https://stackoverflow.com/questions/26558120/may-a-while-loop-be-used-with-yield-in-scala

// iterate "generates the sequence resulting from the repeated application of a function to a start element."
// takeWhile is an iterator returning elements from the object it is called on as long as a condition is satisfied

// Note: could also do Seq.iterate(factor, len: Int){}.takeWhile(_ < 15), but need to know the length of the final
//       set in advance.
