
def isSorted[A](as : Array[A], lt: (A,A) => Boolean) = {

  def loop(i : Int, a : Array[A]) : Boolean = {
    if (i >= a.size - 1) true
    else if (lt(a(i), a(i+1))) loop(i + 1, a)
    else false
  }

  loop(0, as)
}

isSorted(Array(2,3,1), (a:Int,b:Int) => a < b)
isSorted(Array(1,2,3), (a:Int,b:Int) => a < b)
