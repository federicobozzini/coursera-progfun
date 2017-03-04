package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times(\"\")") {
    assert(times(List()) === List())
  }

  test("times(a)") {
    assert(times(List('a')) === List(('a', 1)))
  }

  test("times(a, b)") {
    val t = times(List('a', 'b'))
    assert(t.length == 2)
    assert(t.contains(('a', 1)))
    assert(t.contains(('b', 1)))
  }

  test("times(a, b, a)") {
    val t = times(List('a', 'b', 'a'))
    assert(t.length == 2)
    assert(t.contains(('a', 2)))
    assert(t.contains(('b', 1)))
  }


  test("makeOrderedLeafList for empty table") {
    assert(makeOrderedLeafList(List()) === List())
  }


  test("makeOrderedLeafList for small ordere table") {
    assert(makeOrderedLeafList(List(('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('x',3)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine of some small leaf list") {
    val leaflist = List(Leaf('v', 3))
    assert(combine(leaflist) === List(Leaf('v', 3)))
  }

  test("combine of some simple leaf list" +
    "mple leaf list") {
    val leaflist = List(Leaf('v', 3), Leaf('t', 2))
    assert(combine(leaflist) === List(Fork(Leaf('v',3),Leaf('t',2),List('v', 't'),5)))
  }
  test("combine of some small fork list" +
    "mple leaf list") {
    val leaflist = List(Fork(Leaf('v',3),Leaf('t',2),List('v', 't'),5))
    assert(combine(leaflist) === List(Fork(Leaf('v',3),Leaf('t',2),List('v', 't'),5)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("until of simple instruction") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode no text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val secret: List[Bit] = List()
    assert(decode(leaflist, secret).mkString == "")
  }

  test("decode simple text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val secret: List[Bit] = List(1)
    assert(decode(leaflist, secret).mkString == "d")
  }

  test("decode simple text 2") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val secret: List[Bit] = List(0,1)
    assert(decode(leaflist, secret).mkString == "b")
  }

  test("decode text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val secret: List[Bit] = List(0,1,0,0,1,0,0)
    assert(decode(leaflist, secret).mkString == "bada")
  }

  test("decoded secret") {
    assert(decodedSecret.mkString == "huffmanestcool")
  }


  test("encode no text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "".toList
    val secret: List[Bit] = List()
    assert(encode(leaflist)(str) == secret)
  }

  test("encode simple text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "d".toList
    val secret: List[Bit] = List(1)
    assert(encode(leaflist)(str) == secret)
  }

  test("encode simple text 2") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "b".toList
    val secret: List[Bit] = List(0,1)
    assert(encode(leaflist)(str) == secret)
  }

  test("encode text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "bada".toList
    val secret: List[Bit] = List(0,1,0,0,1,0,0)
    assert(encode(leaflist)(str) == secret)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }
  test("quickEncode no text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "".toList
    val secret: List[Bit] = List()
    assert(quickEncode(leaflist)(str) == secret)
  }

  test("quickEncode simple text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "d".toList
    val secret: List[Bit] = List(1)
    assert(quickEncode(leaflist)(str) == secret)
  }

  test("quickEncode simple text 2") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "b".toList
    val secret: List[Bit] = List(0,1)
    assert(quickEncode(leaflist)(str) == secret)
  }

  test("quickEncode text") {
    val leaflist = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    val str = "bada".toList
    val secret: List[Bit] = List(0,1,0,0,1,0,0)
    assert(quickEncode(leaflist)(str) == secret)
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
