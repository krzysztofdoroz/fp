
def size[A](t : Tree[A]) : Int = {
  t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }
}

def maximum(t : Tree[Int]) : Int = {

  def maxp(t : Tree[Int])(maxVal : Int) : Int = {
    t match {
      case Leaf(x) => math.max(x, maxVal)
      case Branch(l, r) => math.max(maxp(l)(maxVal), maxp(r)(maxVal))
    }
  }
  maxp(t)(Int.MinValue)
}

def depth[A](t : Tree[A]) : Int = {
  t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + math.max(depth(l), depth(r))
  }
}


val tree = Branch(Branch(Leaf(2),Leaf(3)), Leaf(4))
size(tree)
maximum(tree)
depth(tree)