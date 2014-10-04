

// ex 9

def length[A](l : List[A]) : Int = {
  l.foldRight(0)((_,b) => b + 1)
}

length(List(1,2))
length(Nil)
length(List(1))
List(1.0,2.0,3.0).foldLeft(2.0)((acc: Double,x) => math.pow(acc,x))
List(1.0,2.0,3.0).foldRight(2.0)((acc: Double,x) => math.pow(acc,x))
