
case class SecretHandshake() {

  private var ACTIONS = List("wink", "double blink", "close your eyes", "jump", "reverse")

  def toBinary(number: Int, binary: Seq[Int] = Nil): Seq[Int] = {

    if (number == 0) {
      binary.reverse
    } else {
      val remainder = number % 2
      val updated_binary = binary :+ remainder
      val updated_number = number / 2
      toBinary(updated_number, updated_binary)
      // toBinary(number / 2, binary :+ (number % 2))
    }

  }

  def commands(input: Int): List[String] = {

    val binary_input = toBinary(input)

    val possible_actions = ACTIONS.take(binary_input.length)

    val commanded_actions = binary_input.zip(possible_actions).filter(_._1 == 1).unzip._2

    if (commanded_actions.contains("reverse")) {
      commanded_actions.reverse.tail.toList
    } else {
      commanded_actions.toList
    }
  }

}

// https://runestone.academy/runestone/books/published/pythonds/BasicDS/ConvertingDecimalNumberstoBinaryNumbers.html
// https://stackoverflow.com/questions/29057360/scala-filter-listint-which-exists-in-other-list-of-tuples
// http://allaboutscala.com/tutorials/chapter-8-beginner-tutorial-using-scala-collection-functions/scala-zip-example/