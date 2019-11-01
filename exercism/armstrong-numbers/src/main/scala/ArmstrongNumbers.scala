object ArmstrongNumbers {

  def isArmstrongNumber(num: Int): Boolean = {

    val num_array = num.toString.map(_.asDigit)
    val power_sum = num_array.foldLeft(0.0)((x, y) => x + scala.math.pow(y, num_array.length))

    num == power_sum

  }

}