package ru.spbau.jvm.scala.task04


sealed trait SeqMultiSet[+A] {
  def ++[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B]

  def --[B >: A](es: B*): SeqMultiSet[B]

  def *(t: Int): SeqMultiSet[A]

  def filter(p: A => Boolean): SeqMultiSet[A]

  def withFilter(p: A => Boolean): SeqMultiSet[A]

  def map[B](f: A => B): SeqMultiSet[B]

  def flatMap[B](f: A => SeqMultiSet[B]): SeqMultiSet[B]

  def apply[B >: A](e: B): Int

  def find[B >: A](e: B): Option[B]

  def &[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B]

  def |[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B]

  def &~[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B]
}

case object Nil extends SeqMultiSet[Nothing] {
  override def ++[B >: Nothing](ts: SeqMultiSet[B]): SeqMultiSet[B] = ts

  override def --[B >: Nothing](es: B*): SeqMultiSet[B] = Nil

  override def *(t: Int): SeqMultiSet[Nothing] = Nil

  override def filter(p: (Nothing) => Boolean): SeqMultiSet[Nothing] = Nil

  override def withFilter(p: (Nothing) => Boolean): SeqMultiSet[Nothing] = Nil

  override def map[B](f: (Nothing) => B): SeqMultiSet[B] = Nil

  override def flatMap[B](f: (Nothing) => SeqMultiSet[B]): SeqMultiSet[B] = Nil

  override def apply[B >: Nothing](e: B): Int = 0

  override def find[B >: Nothing](e: B): Option[B] = None

  override def &[B >: Nothing](ts: SeqMultiSet[B]): SeqMultiSet[B] = Nil

  override def |[B >: Nothing](ts: SeqMultiSet[B]): SeqMultiSet[B] = ts

  override def &~[B >: Nothing](ts: SeqMultiSet[B]): SeqMultiSet[B] = Nil
}

case class Nod[+A](v: A, c: Int = 1, n: SeqMultiSet[A] = Nil) extends SeqMultiSet[A] {
  override def ++[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B] = Nod(v, c, n ++ ts)

  override def --[B >: A](es: B*): SeqMultiSet[B] = (if (es.contains(v)) Nil else Nod(v, c)) ++ n.--(es: _*)

  override def *(t: Int): SeqMultiSet[A] = if (t <= 0) Nil else Nod(v, c * t, n * t)

  override def filter(p: (A) => Boolean): SeqMultiSet[A] = (if (p(v)) Nod(v, c) else Nil) ++ n.filter(p)

  override def withFilter(p: (A) => Boolean): SeqMultiSet[A] = (if (p(v)) Nod(v, c) else Nil) ++ n.withFilter(p)

  override def map[B](f: (A) => B): SeqMultiSet[B] = Nod(f(v), c, n.map(f))

  override def flatMap[B](f: (A) => SeqMultiSet[B]): SeqMultiSet[B] = (f(v) * c) ++ n.flatMap(f)

  override def apply[B >: A](e: B): Int = if (e == v) c else n(e)

  override def find[B >: A](e: B): Option[B] = if (e == v) Some(e) else n.find(e)

  override def &[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B] = (Nod(v) * c.min(ts(v))) ++ (n & ts)

  override def |[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B] = (Nod(v) * (c + ts(v))) ++ (n | (ts -- v))

  override def &~[B >: A](ts: SeqMultiSet[B]): SeqMultiSet[B] = (Nod(v) * (c - ts(v))) ++ (n &~ ts)
}

object SeqMultiSet {
  def apply[A](es: A*): SeqMultiSet[A] =
    es.groupBy[A](identity).foldRight[SeqMultiSet[A]](Nil)((m, s) => Nod(m._1, m._2.length) ++ s)

  def unapplySeq[Q](s: SeqMultiSet[Q]): Option[Seq[Q]] =
    s match {
      case Nil => None
      case Nod(v, c, n) => Some(Seq.fill(c)(v) ++ unapplySeq(n).getOrElse(Seq.empty))
    }
}