

def increment(a : List[Int]) : List[Int] = {
  a.foldRight(List[Int]())((x, acc) =>  (x + 1) :: acc)
}

val l = List(1,2,3)
increment(l)
