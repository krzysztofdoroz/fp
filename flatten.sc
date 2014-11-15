

def flatten[A](l : List[List[A]]) : List[A] = {
  def append[A](a : List[A], b : List[A]) : List[A] = {
    b.foldLeft(a.reverse)((acc, x) => x :: acc ).reverse
  }

  l.foldLeft(List[A]())(append)

}

flatten(List(List(1,2), List(3,4)))
flatten(List(List(1,2), Nil))
flatten(List(List(1,2), List(3), List(4)))
