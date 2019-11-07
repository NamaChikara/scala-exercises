
case class Matrix(input: String = "") {

  private val matrix_rows = input
    .split("\\n")
    .map(_.split(" "))

  def row(r: Int): Vector[Int] = {

    matrix_rows(r).map(_.toInt).toVector

  }

  def column(c: Int): Vector[Int] = {

    for (r <- matrix_rows.toVector) yield r(c).toInt

  }

}

