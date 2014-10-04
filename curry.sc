
def curry[A,B,C](f : (A,B) => C): A => (B => C) = {
  (a : A) => ((b : B) => f(a,b))
}


val f = curry((a:Int, b:Int) => a+b)
val fp = f(1)
val fpp = fp(2)
