import scala.annotation.tailrec

sealed trait Stream[+A] {

  def toList: List[A] = {

    @tailrec
    def loop(a : Stream[A], acc : List[A]): List[A] = {
      a match {
        case Empty => acc.reverse
        case Cons(head, tail) => loop(tail() ,head() :: acc)
      }
    }

    loop(this, List())
  }

  def take(n : Int): Stream[A] = {

    if (n > 0) this match {
      case Cons(h, t) if (n == 1) => Stream.cons(h(), Empty)
      case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
      case Empty => Empty
    }
    else
      Stream()
  }

  def takeWhile(p : A => Boolean): Stream[A] = {

     this match {
       case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
       case _ => Empty
     }
  }

  def drop(n : Int): Stream[A] = {

    @tailrec
    def loop(s : Stream[A], n : Int): Stream[A] = {
      if (n > 0) s match {
        case Cons(h, t) => loop(t(), n - 1)
        case Empty => Empty
      }
      else
        s
    }

    loop(this, n)
  }

  //imp
  def foldRight[B](z : => B)(f : (A, => B) => B) : B = {
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def forAll(p : A => Boolean) : Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhile2(p : A => Boolean): Stream[A] = {
    foldRight(Empty : Stream[A])((a, b) => if(p(a)) Stream.cons(a,b) else Stream.empty)
  }

  def headOption: Option[A] = {
    foldRight(None : Option[A])((a,b) => Some(a))
  }

  def map[B](f : A => B) : Stream[B] = {
    foldRight(Empty : Stream[B])((a,b) => Stream.cons(f(a), b))
  }

  def filter(p : A => Boolean) : Stream[A] = {
    foldRight(Empty : Stream[A])((a,acc) => if(p(a)) Stream.cons(a, acc) else acc)
  }

  def append[B >:A](s : => Stream[B]) : Stream[B] = {
    foldRight(s)((a, acc) => Stream.cons(a,acc))
  }

  def flatMap[B](f : A => Stream[B]) : Stream[B] = {
    foldRight(Empty : Stream[B])((a, acc) => f(a) append acc)
  }

  def constant[A](a : A) : Stream[A] = {
     Stream.cons(a, constant(a))
  }

  def from(n : Int) : Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  def fib(): Stream[Int] = {

    def gen(a : Int, b : Int): Stream[Int] = {
      Stream.cons(a, gen(b, a + b))
    }

    gen(0,1)
  }

  //imp
  def unfold[A,S](z : S)(f: S => Option[(A,S)]) : Stream[A] = {
    f(z) match {
      case None => Empty : Stream[A]
      case Some((a,s)) => Stream.cons(a, unfold(s)(f))
    }
  }

  def fib2(): Stream[Int] = {
    unfold((0,1)){case (a : Int,b : Int) => Some((a,(b, a + b)))}
  }

  def from2(n : Int) : Stream[Int] = {
    unfold(n)((a : Int) => Some((a, a + 1)))
  }

  def constant2[A](a : A) : Stream[A] = {
    unfold(a)((a : A) => Some((a,a)))
  }

  def ones2() : Stream[Int] = {
    unfold(1)(_ => Some((1,1)))
  }

  def mapViaUnfold[B](f : A => B) : Stream[B] = {
    unfold(this){
      case Cons(h,t) => Some((f(h()), t()))
      case Empty => None
    }
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl : => Stream[A]) : Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A] : Stream[A] = Empty

  def apply[A](as : A*): Stream[A] = {
    if(as.isEmpty) empty else cons(as.head, apply(as.tail : _*))
  }

}

//tests
