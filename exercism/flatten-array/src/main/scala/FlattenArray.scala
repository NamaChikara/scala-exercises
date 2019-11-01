object FlattenArray {

  def flatten(input: Any, L: List[Int] = Nil): List[Int] = {

    if (input.isInstanceOf[List[Any]]) {
      input.asInstanceOf[List[Any]].foldRight(L)(flatten)
    } else if (input.isInstanceOf[Int]) {
      input.asInstanceOf[Int] :: L
    } else {
      L
    }

  }

}

// https://stackoverflow.com/questions/19386964/i-want-to-get-the-type-of-a-variable-at-runtime