import scala.util.matching.Regex

object Bob {

  val LETTER_REGEX: Regex = new Regex("[a-zA-Z]")

  private def is_whitespace(statement: String): Boolean = {
    statement.replaceAll("\\s", "").length == 0
  }

  private def is_question(statement: String): Boolean = {
    statement.replaceAll("\\s", "").endsWith("?")
  }

  private def is_numbers(statement: String): Boolean = {
    LETTER_REGEX.findFirstIn(statement).getOrElse("1") == "1"
  }

  private def is_uppercase(statement: String): Boolean = {
    statement.toUpperCase == statement
  }

  def response(statement: String): String = {

    if (is_whitespace(statement)) {
      "Fine. Be that way!"
    } else if (is_question(statement)) {
      if (is_uppercase(statement) & !is_numbers(statement)) {
        "Calm down, I know what I'm doing!"
      } else {
        "Sure."
      }
    } else if (is_uppercase(statement) & !is_numbers(statement)) {
      "Whoa, chill out!"
    } else {
      "Whatever."
    }

  }

}
