

def flatten[A](l : List[List[A]]) : List[A] = {

  def append[A](a : List[A], b : List[A]) : List[A] = {
    b.foldLeft(a.reverse)((acc, x) => x :: acc ).reverse
  }

  l.foldLeft(List[A]())(append)
}

def map[A,B](as : List[A])(f : A => B) : List[B] = {

  def loop(as : List[A], acc: List[B])(f : A => B) : List[B] = {
    as match {
      case Nil => acc
      case x::xs => loop(xs, f(x) :: acc)(f)
    }
  }

  loop(as,List[B]())(f).reverse
}

def flatMap[A,B](as : List[A])(f: A => List[B]) : List[B] = {

    flatten(map(as)(f))

}


flatMap(List(1,2,3))(i => List(i,i))
