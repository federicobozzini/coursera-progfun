import forcomp.Anagrams

Anagrams.dictionary.filter(_.length==2).sorted

def v(x:String): Int = x.length

val tt = List("abcd", "e").flatten.mkString
v(tt)

5:: List()
5:: List(Nil)

class Poly(terms0: Map[Int, Double]) {
  val terms = terms0 withDefaultValue 0.0
  def this(bindings: (Int, Double)*) = this(bindings.toMap)

  def +(other: Poly) = new Poly((other.terms foldLeft terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double]  =
    terms.updated(term._1, term._2 + terms(term._1))

  override def toString: String =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
}

val x = new Poly(4-> 1, 2->2, 0-> 5)
val y = new Poly(4-> 3, 3->1, 0-> 2)
val z = x + y

val t = z.terms

t(4) == 4
t(2) == 2
t(3) == 1
t(0) == 7

"hello" groupBy(identity) mapValues (x => x.length)

val occurrences = List(('a', 1), ('b', 2))

val p = for {
  i <- 0 until occurrences.length
  o2 = occurrences.updated(i, (occurrences(i)._1, occurrences(i)._2 - 1)).filter(x => x._2 > 0)
} yield o2

p.toList

Nil == ""
List() == List(Nil)

for {
  i <- 0 until 5
  if (i > 6)
  j <- 0 until i
} yield i