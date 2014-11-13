
def dropWhile[A](l : List[A], f : A => Boolean) : List[A] = {

  def loop[A](l : List[A], f : A => Boolean) : List[A] = {
      l match {
        case Nil => Nil
        case x::xs if f(x) => loop(xs, f)
        case _ => l
    }
  }

  loop(l, f)
}


dropWhile(List(2,3,4), (a : Int) => a % 2 == 0)


dropWhile(List(2,6,4), (a : Int) => a % 2 == 0)


dropWhile(Nil, (a : Int) => a % 2 == 0)
