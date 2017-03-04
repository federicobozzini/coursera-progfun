package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite  {

  test("wordOccurrences: ''") {
    assert(wordOccurrences("") === List())
  }

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }


  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: Linux rulez") {
    assert(sentenceOccurrences(List("Linux", "rulez")) === List(('e', 1), ('i', 1), ('l', 2), ('n', 1), ('r', 1), ('u', 2), ('x', 1), ('z', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("dictionaryByOccurrences.get: ''") {
    assert(dictionaryByOccurrences.get(List()).map(_.toSet) === None)
  }

  test("dictionaryByOccurrences with default.get: ''") {
    assert((dictionaryByOccurrences withDefaultValue List())(List()) === List())
  }

  test("dictionaryByOccurrences with default.get: at") {
    assert((dictionaryByOccurrences withDefaultValue List())(List(('a', 1), ('t', 1))) === List("at"))
  }

  test("word anagrams: ''") {
    assert(wordAnagrams("") === List())
  }


  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("word anagrams: zulu") {
    assert(wordAnagrams("zulu").toSet === Set("Zulu"))
  }

  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: ''") {
    assert(combinations(List()).toSet === Set(List()))
  }

  test("combinations: a") {
    val occurrences = List(('a', 1))
    val out = List(List(('a', 1)), List())
    assert(combinations(occurrences).toSet === out.toSet)
  }


  test("combinations: List(('a', 2), ('b', 2))") {
    val in = List(('a', 2), ('b', 2))
    val res = List(
            List(),
            List(('a', 1)),
            List(('a', 2)),
            List(('b', 1)),
            List(('a', 1), ('b', 1)),
            List(('a', 2), ('b', 1)),
            List(('b', 2)),
            List(('a', 1), ('b', 2)),
            List(('a', 2), ('b', 2))
            )
    val out = combinations(in)
    assert(out.toSet == res.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: laad - a") {
    val laad = List(('a', 2), ('d', 1), ('l', 1))
    val a = List(('a', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(laad, a) === lad)
  }


  test("subtract: lard - lard") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard, lard) === List())
  }

  test("subtract: lard - ''") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val empty = List()
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, empty) === lard)
  }

  test("subtract: llaarrdd - lard") {
    val lard2 = List(('a', 2), ('d', 2), ('l', 2), ('r', 2))
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    assert(subtract(lard2, lard) === lard)
  }

  test("subtract: llaarrdd - lard - a") {
    val lard2 = List(('a', 2), ('d', 2), ('l', 2), ('r', 2))
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(subtract(lard2, lard),r) === lad)
  }

  test("subtract: rexnil - nil") {
    val rexnil = List(('e', 1), ('i', 1), ('l', 1), ('n', 1), ('r', 1), ('x', 1))
    val rex = List(('e', 1), ('r', 1), ('x', 1))
    val nil = List(('i', 1), ('l', 1), ('n', 1))
    assert(subtract(rexnil, rex) === nil)
  }



  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }


  test("sentence anagrams: []") {
    val sentence = List()
    assert(sentenceAnagrams(sentence) === List(Nil))
  }


  test("sentence anagrams: 'at'") {
    val sentence = List("at")
    val anas = List(
      List("at")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: 'et'") {
    val sentence = List("et")
    val anas = List(
      List("et")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: 'sad'") {
    val sentence = List("sad")
    val anas = List(
      List("sad"),
      List("ads")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: 'cats'") {
    val sentence = List("cats")
    val anas = List(
      List("cats"),
      List("Acts"),
      List("cast")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: 'te', 'a'") {
    val sentence = List("te", "a")
    val anas = List(
      List("tea"),
      List("ate"),
      List("eat")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: fare") {
    val sentence = List("fare")
    val anas = List(
      List("fare"),
      List("fear")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }


  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    val anas = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
    assert(sentenceAnagrams(sentence).toSet === anas.toSet)
  }

}
