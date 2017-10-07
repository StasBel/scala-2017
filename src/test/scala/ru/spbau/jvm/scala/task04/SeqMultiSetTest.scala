package ru.spbau.jvm.scala.task04

import org.scalatest.FunSuite

class SeqMultiSetTest extends FunSuite {
  val s = SeqMultiSet(1, 2, 1, 3, 4, 5, 5, 5) * 3

  test("search") {
    assert(s(5) == 9, "3 five's time 3")
    assert(s(1) == 6, "2 one's time 3")
    assert(s(4) == 3, "1 four time 3")
    assert(s(0) == 0, "no zeros")
    assert(s.find(0).isEmpty, "no zeros")
    assert(s.find(2).isDefined, "has two")
    assert(s.find(2).contains(2), "has two")
  }

  test("for-comprehension and funcs") {
    val es = for (e <- s if e % 2 == 0) yield {
      e * e
    }
    assert(es == SeqMultiSet(4, 16) * 3, "filter even then square")
    assert(s.filter(_ < 3).map(_ * 2).find(4).isDefined, "find 4 after transofrmations")
  }

  test("pattern") {
    assert((s match {
      case Nil => 0
      case Nod(_, 10, _) => 1
      case Nod(5, _, _) => 2
      case Nod(_, _, Nil) => 3
      case _ => 4
    }) != 0, "pattern matching examples")
  }
}
