
// ex 5

def compose[A,B,C](f: B => C, g : A => B): A => C = {
  (a: A) => f(g(a))
}

val x = List(1,2,3,4,5) match {
  case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
}

