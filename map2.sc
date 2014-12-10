
def map2[A,B,C](a : Option[A], b : Option[B])(f : (A,B) => C) : Option[C] = {
  (a,b) match {
    case (None,_) | (_, None) => None
    case (Some(x), Some(y)) => Some(f(x,y))
  }
}

val a = Some(1)
val b = Some(2)
val n = None
val f = (a: Int, b: Int) => a + b
map2(a,b)(f)
map2(a,n)(f)
map2(n,b)(f)


