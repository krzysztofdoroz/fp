
def sum(a : List[Int]): Int = {
  a.foldLeft(0)((acc : Int, x : Int) => acc + x)
}

sum(List(1,2,3))
sum(Nil)

def product(a : List[Int]): Int = {
  a.foldLeft(1)((acc : Int, x : Int) => acc * x)
}

product(List(2,2,3))
product(Nil)

def len(a : List[Int]): Int = {
  a.foldLeft(0)((acc : Int, x : Int) => acc + 1)
}

product(List(2,2,3))
product(Nil)
