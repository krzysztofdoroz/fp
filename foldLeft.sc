
// ex 10

def foldLeft[A,B](l : List[A], z : B)(f : (B,A) => B): B = {
  l match {
    case Nil => z
    case x::xs => foldLeft(xs, f(z,x))(f)
  }
}

foldLeft(List(1.0,2.0,3.0),2.0)((acc: Double,x) => math.pow(acc,x))
foldLeft(List("a","b","c"),"z")((acc: String, x : String) => acc + x)

