
def filter[A](as : List[A])(f: A => Boolean) : List[A] = {

  def loop(as : List[A], acc : List[A])(f: A => Boolean) : List[A] = {
    as match {
      case Nil => acc
      case x::xs if f(x) => loop(xs, x :: acc)(f)
      case _::xs => loop(xs, acc)(f)
    }
  }

  loop(as, List[A]())(f).reverse
}



filter(List(1,2,3))((a : Int) => a % 2 == 1)


filter(List(2,2,4))((a : Int) => a % 2 == 1)


filter(Nil)((a : Int) => a % 2 == 1)

