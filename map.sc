
def map[A,B](as : List[A])(f : A => B) : List[B] = {

  def loop(as : List[A], acc: List[B])(f : A => B) : List[B] = {
    as match {
      case Nil => acc
      case x::xs => loop(xs, f(x) :: acc)(f)
    }
  }

  loop(as,List[B]())(f).reverse
}


map(List(1,2,3))((a : Int) => a + 2)


