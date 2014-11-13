
def drop[A](l : List[A], n : Int) : List[A] = {

  def loop[A](l : List[A], n : Int) : List[A] = {
    (l,n) match {
      case (Nil, _) => Nil
      case (_, 0) => l
      case (x::xs, n) => loop(xs, n - 1)
    }
  }

  loop(l, n)
}

drop(List(1,2), 1)

drop(Nil, 4)

drop(List(1,2), 4)

