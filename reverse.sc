
def reverse[A](l : List[A]) : List[A] = {
  l.foldLeft(List[A]())((acc, x) => x::acc)
}

reverse(List(1,2,3))


