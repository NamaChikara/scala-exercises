object Etl {

  def transform(legacy: Map[Int, Seq[String]]): Map[String, Int] = {

    var current = Map.empty[String, Int]

    for ((k, v) <- legacy) {
      // v, the value variable is a sequence - we can iterate over
      // its values using the foreach method.  Note that the test
      // suite shows that the solution should have lowercase keys.
      v.foreach(x => current += (x.toLowerCase -> k))
    }

    current
  }

}