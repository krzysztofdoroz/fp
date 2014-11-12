

def tail[A](a : List[A]) : List[A] = {
     a match {
       case Nil => Nil
       case _::as => as
     }
}

tail(List(1,2,3))
tail(List(1))
tail(List())
tail(Nil)