package lab4

class AnagramsSuite extends munit.FunSuite:

  import Anagrams.*

  test("wordOccurrences: abcd") {
    assertEquals(wordOccurrences("abcd"), List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assertEquals(wordOccurrences("Robert"), List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("sentenceOccurrences: abcd e") {
    assertEquals(sentenceOccurrences(List("abcd", "e")), List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("dictionaryByOccurrences.get: eat") {
    assertEquals(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet), Some(Set("ate", "eat", "tea")))
  }

  test("wordAnagrams married") {
    assertEquals(wordAnagrams("married").toSet, Set("married", "admirer"))
  }

  test("wordAnagrams player") {
    assertEquals(wordAnagrams("player").toSet, Set("parley", "pearly", "player", "replay"))
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assertEquals(subtract(lard, r), lad)
  }

  test("combinations: []") {
    assertEquals(combinations(Nil), List(Nil))
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
    assertEquals(combinations(abba).toSet, abbacomb.toSet)
  }

  test("sentence anagrams: []") {
    val sentence = List()
    assertEquals(sentenceAnagrams(sentence), List(Nil))
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
    assertEquals(sentenceAnagrams(sentence).toSet, anas.toSet)
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds
