def sum(f: Int => Int, a: Int, b: Int): Int = {
  def loop(a: Int, acc: Int): Int = {
    if (a>b) acc
    else loop(a+1, f(a)+acc)
  }
  loop(a, 0)
}

def product(a: Int, b: Int): Int =
  if (a>b) 1
  else a*product(a+1, b)

def mapReduce(f:(Int, Int) => Int, zero:Int)(a: Int, b: Int) = {
  def loop(a: Int, acc: Int): Int = {
    if (a > b) acc
    else loop(a + 1, f(a,acc))
  }
  loop(a, zero)
}

def product2(a: Int, b: Int) = mapReduce((x, y)=> x*y, 1)(a, b)

def factorial(n:Int) = product2(1, n)

factorial(6)

product2(1, 3)

8%2