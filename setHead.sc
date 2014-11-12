
def setHead[A](a : List[A], b : A) : List[A] = {
  a match {
    case Nil => List(b)
    case _::xs => b::xs
  }
}

setHead(List(1,2), 4)

setHead(Nil, 42)

setHead(List("asa", "aswer"), 4)
