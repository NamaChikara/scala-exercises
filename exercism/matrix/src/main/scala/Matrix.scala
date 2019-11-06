
case class Matrix(input: String = "") {

  val NEWLINE_REGEX = "\\n".r

  val all_values = input
    .split("\\n")
    .map(_.split(" "))
    .flatten.map(_.toInt)
    .zipWithIndex

  val num_rows = NEWLINE_REGEX.findAllIn(input).length + 1

  val num_cols = all_values.length / num_rows

  def row(r: Int): Vector[Int] = {

    all_values
      .filter(_._2 < (r + 1) * num_cols)
      .filter(_._2 >= r * num_cols)
      .map({ case(a, b) => a })
      .toVector

  }

  def column(c: Int): Vector[Int] = {

    all_values
      .filter(_._2 % num_cols == c)
      .map({ case(a, b) => a})
      .toVector

  }

}

