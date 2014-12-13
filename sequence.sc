import scala.annotation.tailrec
def sequence[A](a : List[Option[A]]) : Option[List[A]] = {

  @tailrec
  def loop[A](xs : List[Option[A]], acc : List[A]) : Option[List[A]] = {
    xs match {
      case Some(x)::t => loop(t, x :: acc)
      case None::t => None
      case Nil => Some(acc.reverse)
    }
  }

  loop(a, List())
}
val op = Some(2)
val opp = Some(3)
val n = None
sequence(List(op,opp,n))
sequence(List(op,opp))
sequence(List(n))
sequence(List(opp))


