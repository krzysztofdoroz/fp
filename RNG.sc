trait RNG {
  def nextInt: (Int, RNG)


}

object RNG {

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = SimpleRNG(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  def nonNegativeInt(rng: RNG) : (Int, RNG) = {
    val (v, rng2) = rng.nextInt
    if (v == Int.MinValue)
      (0, rng2)
    else if (v < 0)
      (-v, rng2)
    else
      (v, rng2)
  }

  def double(rng : RNG) : (Double, RNG) = {
    val max = Int.MaxValue
    val (v,rng2) = nonNegativeInt(rng)

    (v.toDouble/max, rng2)
  }


}

type Rand[+A] = RNG => (A, RNG)

def unit[A](a : A) : Rand[A] = {
  rng => (a, rng)
}

def map[A,B](s : Rand[A])(f: A => B): Rand[B] = {
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }
}

def double: Rand[Double] = {
  map(RNG.nonNegativeInt)( _ / (Int.MaxValue.toDouble + 1))
}

def int: Rand[Int] = {
  rnd => {
    rnd.nextInt
  }
}

def map2[A,B,C](ra : Rand[A], rb : Rand[B])(f : (A,B) => C) : Rand[C] = {
  rng => {
    val (a,rng1) = ra(rng)
    val (b,rng2) = rb(rng1)
    (f(a,b), rng2)
  }
}

def flatMap[A,B](f : Rand[A])(g : A => Rand[B]) : Rand[B] = {
   rng => {
     val (v, rng2) = f(rng)
     g(v)(rng2)
   }
}

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
  rnd => {
    val (l, r) = fs.foldLeft[(List[A], RNG)]((List[A](),rnd)) ((acc, a) => (a(acc._2)._1 :: acc._1, a(acc._2)._2))
    (l.reverse, r)
  }
}
//tests
RNG.nonNegativeInt(RNG.SimpleRNG(12))
RNG.double(RNG.SimpleRNG(12))
double(RNG.SimpleRNG(12))
int(RNG.SimpleRNG(12))
sequence(List(int, int))(RNG.SimpleRNG(12))




