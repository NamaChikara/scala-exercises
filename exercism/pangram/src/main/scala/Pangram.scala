object Pangrams {

  def isPangram(input: String): Boolean = {

    input
      .toLowerCase
      .filter(x => x >= 'a' && x <= 'z')
      .toSet
      .size
      .==(26)

  }
}

