
def zipWith[A,B](a : List[A], b : List[A])(f : (A, A) => B) : List[B] = {

  // TODO: check if lists have equal length

  def loop[A,B](a : List[A], b : List[A], acc : List[B])(f : (A, A) => B) : List[B] = {
    (a, b) match {
      case (Nil, Nil) => acc
      case (x::xs, y::ys) => loop(xs, ys, f(x,y) :: acc)(f)
      case (Nil, _) | (_, Nil) => acc
    }
  }

  loop(a, b, List[B]())(f).reverse
}
zipWith(List(1,1), List(2,2))((a : Int,b : Int) => a + b)

zipWith(List(1,1), List(2,2))((a, b) => (a,b))

