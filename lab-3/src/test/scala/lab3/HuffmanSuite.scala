package lab3

class HuffmanSuite extends munit.FunSuite:

  import Huffman.*

  trait TestTrees:
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)

    val text0 = "encoreuntextetressecret".toList
    val bits0 = List(1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1)

    // tree1 is the code tree for text1 (computed using `createCodeTree` of our solution)
    val text1 = "Contrary to popular belief, Lorem Ipsum is not simply random text. It has roots in a piece of classical Latin literature from 45 BC, making it over 2000 years old. Richard McClintock, a Latin professor at Hampden-Sydney College in Virginia, looked up one of the more obscure Latin words, consectetur, from a Lorem Ipsum passage, and going through the cites of the word in classical literature, discovered the undoubtable source."
    val tree1 = Fork(Fork(Fork(Fork(Fork(Leaf('h', 8), Fork(Leaf('y', 5), Leaf('L', 5), List('y', 'L'), 10), List('h', 'y', 'L'), 18), Leaf('n', 19), List('h', 'y', 'L', 'n'), 37), Fork(Fork(Leaf('p', 10), Leaf('m', 11), List('p', 'm'), 21), Leaf('s', 21), List('p', 'm', 's'), 42), List('h', 'y', 'L', 'n', 'p', 'm', 's'), 79), Fork(Fork(Fork(Fork(Fork(Leaf('.', 3), Leaf('k', 3), List('.', 'k'), 6), Fork(Fork(Leaf('V', 1), Fork(Leaf('-', 1), Leaf('S', 1), List('-', 'S'), 2), List('V', '-', 'S'), 3), Leaf('I', 3), List('V', '-', 'S', 'I'), 6), List('.', 'k', 'V', '-', 'S', 'I'), 12), Leaf('u', 12), List('.', 'k', 'V', '-', 'S', 'I', 'u'), 24), Leaf('i', 24), List('.', 'k', 'V', '-', 'S', 'I', 'u', 'i'), 48), Fork(Fork(Leaf('d', 12), Fork(Fork(Leaf('0', 3), Fork(Leaf('v', 2), Leaf('w', 2), List('v', 'w'), 4), List('0', 'v', 'w'), 7), Leaf('f', 7), List('0', 'v', 'w', 'f'), 14), List('d', '0', 'v', 'w', 'f'), 26), Leaf('t', 26), List('d', '0', 'v', 'w', 'f', 't'), 52), List('.', 'k', 'V', '-', 'S', 'I', 'u', 'i', 'd', '0', 'v', 'w', 'f', 't'), 100), List('h', 'y', 'L', 'n', 'p', 'm', 's', '.', 'k', 'V', '-', 'S', 'I', 'u', 'i', 'd', '0', 'v', 'w', 'f', 't'), 179), Fork(Fork(Fork(Leaf('a', 26), Leaf('r', 27), List('a', 'r'), 53), Fork(Fork(Leaf('c', 14), Fork(Leaf('g', 7), Fork(Leaf('C', 4), Leaf('b', 4), List('C', 'b'), 8), List('g', 'C', 'b'), 15), List('c', 'g', 'C', 'b'), 29), Fork(Leaf('l', 15), Fork(Fork(Fork(Fork(Leaf('5', 1), Leaf('B', 1), List('5', 'B'), 2), Fork(Leaf('x', 1), Leaf('4', 1), List('x', '4'), 2), List('5', 'B', 'x', '4'), 4), Fork(Fork(Leaf('M', 1), Leaf('H', 1), List('M', 'H'), 2), Fork(Leaf('2', 1), Leaf('R', 1), List('2', 'R'), 2), List('M', 'H', '2', 'R'), 4), List('5', 'B', 'x', '4', 'M', 'H', '2', 'R'), 8), Leaf(',', 8), List('5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','), 16), List('l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','), 31), List('c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','), 60), List('a', 'r', 'c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ','), 113), Fork(Fork(Leaf('o', 33), Leaf('e', 34), List('o', 'e'), 67), Leaf(' ', 69), List('o', 'e', ' '), 136), List('a', 'r', 'c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ',', 'o', 'e', ' '), 249), List('h', 'y', 'L', 'n', 'p', 'm', 's', '.', 'k', 'V', '-', 'S', 'I', 'u', 'i', 'd', '0', 'v', 'w', 'f', 't', 'a', 'r', 'c', 'g', 'C', 'b', 'l', '5', 'B', 'x', '4', 'M', 'H', '2', 'R', ',', 'o', 'e', ' '), 428)

  test("weight of a simple leaf") {
    assertEquals(weight(Leaf('c', 29)), 29)
  }

  test("weight of a larger tree") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }

  test("chars of a simple leaf") {
    assertEquals(chars(Leaf('u', 2)), List('u'))
  }

  test("chars of a larger tree") {
    new TestTrees:
      assertEquals(chars(t2), List('a', 'b', 'd'))
  }

  test("times of nil") {
    assertEquals(times(List()), List())
  }

  test("times of some word") {
    assertEquals(times("abbrakkadabbrra".toList).sortWith((p1, p2) => p1._2 > p2._2), List(('a', 5), ('b', 4), ('r', 3), ('k', 2), ('d', 1)))
  }

  test("makeOrderedLeafList of nil") {
    assertEquals(makeOrderedLeafList(List()), List())
  }

  test("make ordered leaf list for some frequency table") {
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singletons of Nil") {
    assertEquals(singleton(List()), false)
  }

  test("singletons of a longer list") {
    assert(!singleton(List(Leaf('e', 1), Leaf('t', 2))))
  }

  test("singletons of a list with one tree") {
    new TestTrees:
      assert(singleton(List(t1)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist), List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine of a singleton or nil") {
    val l = List(Leaf('e', 1))
    assertEquals(combine(l), l)
    assertEquals(combine(Nil), Nil)
  }

  test("decoding the secret") {
    assertEquals(decode(frenchCode, secret), "huffmanestcool".toList)
    assertEquals(decodedSecret, "huffmanestcool".toList)
  }

  // this is arguably arguable
  // test("decoding an empty text with a leaf-only code tree") {
  //   assertEquals(decode(new Leaf('c', 10), Nil), List('c'))
  // }

  test("encode some text with frenchCode") {
    new TestTrees:
      assertEquals(encode(frenchCode)(text0), bits0)
  }

  test("decode some bits with frenchCode") {
    new TestTrees:
      assertEquals(decode(frenchCode, bits0), text0)
  }

  test("'createCodeTree(someText)' gives an optimal encoding, the number of bits when encoding 'someText' is minimal") {
    new TestTrees:
      val codeTreeForText1 = createCodeTree(text1.toList)
      assertEquals(encode(codeTreeForText1)(text1.toList).length, 1919)
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("decode and encode a very short text with a larger tree") {
    new TestTrees:
      assertEquals(decode(tree1, encode(tree1)("abu".toList)), "abu".toList)
  }

  test("decode and encode some longer text should be identity") {
    new TestTrees:
      assertEquals(decode(tree1, encode(tree1)("literature from 45 BC, making it over 2000 years old.".toList)), "literature from 45 BC, making it over 2000 years old.".toList)
  }

  test("convert: code table is created correctly") {
    new TestTrees:
      assertEquals(convert(t2).sortWith((p1, p2) => p1._1 < p2._1), List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
  }

  test("quick encode gives the correct byte sequence") {
    val res = List(1, 0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 1)
    new TestTrees:
      assertEquals(quickEncode(tree1)("looked up one of the more obscure Latin words".toList), res)
  }

  test("decode and quick encode is identity") {
    new TestTrees:
      assertEquals(decode(tree1, quickEncode(tree1)("ture from 45 BC, making it over 2000 years old. Richard Mc".toList)), "ture from 45 BC, making it over 2000 years old. Richard Mc".toList)
  }

  test("createCodeTree should be implemented in terms of times, makeOrderedLeafList, singleton, combine and until") {
    class OK extends Throwable

    def throwsOK(method: String, instrumented: Huffman): Unit =
      try
        instrumented.createCodeTree("hello".toList)
        fail(s"createCodeTree should be implemented in terms of $method")
      catch {
        case e: OK => ()
      }

    throwsOK("times", new Huffman {
      override def times(chars: List[Char]): List[(Char, Int)] = throw OK()
    })

    throwsOK("makeOrderedLeafList", new Huffman {
      override def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = throw OK()
    })

    throwsOK("singleton", new Huffman {
      override def singleton(trees: List[CodeTree]): Boolean = throw OK()
    })

    throwsOK("combine", new Huffman {
      override def combine(trees: List[CodeTree]): List[CodeTree] = throw OK()
    })

    throwsOK("until", new Huffman {
      override def until(done: List[CodeTree] => Boolean, merge: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = throw OK()
    })
  }

  import scala.concurrent.duration.*

  override val munitTimeout = 10.seconds
