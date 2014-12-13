
sealed trait Option[+A] {
  def map[B](f : A => B): Option[B] = {
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }

  def flatMap[B](f : A => Option[B]) : Option[B] = {
    map(f) getOrElse(None)
  }

  def getOrElse[B >: A](default: => B) : B = {
    this match {
      case Some(x) => x
      case None => default
    }
  }

  def orElse[B >: A](ob: => Option[B]) : Option[B] = {
    map (Some(_)) getOrElse ob
  }

  def filter(f : A => Boolean): Option[A] = {
    if (map (f) getOrElse false) this else None
  }
}
case class Some[+A](get : A) extends Option[A]
case object None extends Option[Nothing]

def map2[A,B,C](a : Option[A], b : Option[B])(f : (A,B) => C) : Option[C] = {
  (a,b) match {
    case (None,_) | (_, None) => None
    case (Some(x), Some(y)) => Some(f(x,y))
  }
}

def traverse[A,B](a : List[A])(f : A => Option[B]): Option[List[B]] = {
  a.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)( ( _ :: _ )  ))
}

// tests:
val opt = Some(5)
val opt2 = None
opt.map((x : Int) => x + 1)

opt2.map((x : Int) => x + 1)
opt getOrElse(55)
opt2 getOrElse(345)

opt orElse opt2
opt2 orElse opt

opt filter((a : Int) => a % 2 == 0)
opt filter((a : Int) => a % 2 != 0)
opt2 filter((a : Int) => a % 2 == 0)



