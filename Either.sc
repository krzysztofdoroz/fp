import scala.util.Try

sealed trait Either[+E, +A] {
  def map[B](f : A => B): Either[E, B] = {
      this match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      }
  }

  def flatMap[EE >: E, B](f : A => Either[EE, B]) : Either[EE, B] = {
    this match {
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }
  }

  def orElse[EE >: E, B >: A](b : => Either[EE, B]): Either[EE, B] = {
    this match {
      case Left(e) => b
      case Right(v) => Right(v)
    }
  }

  def map2[EE >: E, B, C](b : Either[EE, B])(f : (A, B) => C) : Either[EE,C] = {
    for {
      aa <- this
      bb <- b
    } yield f(aa,bb)
  }


}
case class Left[+E](value : E) extends Either[E, Nothing]
case class Right[+A](value : A) extends Either[Nothing, A]

// seq
def sequence[E, A](es : List[Either[E, A]]): Either[E, List[A]] = {
  //foldLeft will return the last Left(e)
  es.foldRight[Either[E, List[A]]](Right(Nil))( (h, acc) => h.map2(acc)(_ :: _) )
}

//traverse
def traverse[E, A, B](as : List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
  as.foldRight[Either[E, List[B]]](Right(Nil))((h,acc) => f(h).map2(acc)(_ :: _) )
}

// tests
val r = Right(3)
val e = Left("none")
val e2 = Left("none2")
val inc = (x : Int) => x + 1
val add = (x : Int, y : Int) => x + y
def incrementEven (x : Int) : Either[String, Int] = {
  if (x % 2 == 0)
    Right(inc(x))
  else
    Left("not even")
}

r map inc
e map inc
e orElse r
r.map2(r)(add)
r.map2(e)(add)
sequence(List(r,e))
sequence(List(r,e,e2))
sequence(List(r,r))
traverse(List(3,2,2))(incrementEven)
traverse(List(4,2,2))(incrementEven)







