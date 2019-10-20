object Hamming {

  def distance(strand_a: String, strand_b: String): Option[Int] = {
    if (strand_a.length != strand_b.length) {
      None
    } else {
      var diff = 0
      for (i <- 0 until strand_a.length) {
        if (strand_a(i) != strand_b(i)) diff += 1
      }
      Some(diff)
    }
  }
}