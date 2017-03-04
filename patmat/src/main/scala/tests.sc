val l = List(('t', 2), ('e', 1), ('x', 3))

val l2 = l.sortBy(_._2)


val v = List[Int](2, 3, 2, 5)


def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())( f(_) :: _)


val vv = mapFun[Int, Int](v, x => x)



val l3 = l2.map(_._2)

val l4 = l2.map(x => List(x._1,  x._2))

type Row = List[Int]
def Row(xs: Int*) = List(xs: _*)

type Matrix = List[Row]
def Matrix(xs: Row*) = List(xs: _*)

val m = Matrix(Row(1,2,3),
  Row(1,2,3),
  Row(1,2,3))

val a = (3, 5)

val b = List(a)

type NumPair = (Int, Int)

val c:NumPair = (4, 3)

type ListNumPair = List[(Int, Int)]

type ListNum = List[Int]


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 =>
    val (first, rest) = xs span(y => y==x)
    first :: pack(rest)
}

def encode[T](xs: List[List[T]]): List[(T, Int)] = xs match {
  case Nil => Nil
  case y::ys => (y.head, y.length) :: encode(ys)
}

def compress[T](xs: List[(T, Int)]): List[(T, Int)] = {
  def add[T](x:(T, Int), xs: List[(T, Int)]): List[(T, Int)] = xs match {
    case Nil => List(x)
    case y:: ys => if (x._1 == y._1) (x._1, x._2+y._2) :: ys else y:: add(x, ys)
  }
  xs match {
    case Nil=> Nil
    case y:: ys => compress(add(y, ys))
  }
}


val x = pack(List("a", "a", "a", "b", "c", "c", "a"))

pack(List("a", "a", "a", "b", "c", "c", "a")) == List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))

val y = encode(x)

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x, y) => 1 + y)

lengthFun(vv)