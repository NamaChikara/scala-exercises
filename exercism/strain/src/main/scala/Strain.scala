
object Strain {

  // List mixes in the trait Seq, so anywhere a Seq is allowed, a List is too.
  // https://stackoverflow.com/questions/10866639/difference-between-a-seq-and-a-list-in-scala

  def keep[A](input: Seq[A], fun: A => Boolean): Seq[A] = {
    input.filter(fun)
  }

  def discard[A](input: Seq[A], fun: A => Boolean): Seq[A] = {
    input.filter(!fun(_))
  }

}