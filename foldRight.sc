

def foldRight[A,B](l : List[A], z : B)(f : (B,A) => B): B = {
  l.reverse.foldLeft(z)(f)
}

foldRight(List(1.0,2.0,3.0),2.0)((acc: Double,x) => math.pow(acc,x))
foldRight(List("a","b","c"),"z")((acc: String, x : String) => acc + x)

