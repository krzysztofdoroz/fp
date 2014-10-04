import scala.annotation.tailrec
def fib(n : Int) : Int =
{

  @tailrec
  def loop(i : Int, N: Int, a : Int, b: Int) : Int =
  {
      N match {
        case 0 => 0
        case 1 => 1
        case `i` => a
        case _ => loop(i + 1, N, b, a + b)
      }
  }

  loop(0,n,0,1)
}

fib(0)
fib(2)
fib(3)
fib(4)
fib(5)
