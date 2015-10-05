package com.squeryl.jdip.monads

import scalaz.Functor
import scalaz.Forall
import scalaz.Free._
import scala.annotation.tailrec

case class Tower[S]()

sealed trait ST[S, A] {
  def flatMap[B](f: A => ST[S, B]): ST[S, B] = FlatMap(this, f)
  def map[B](f: A => B): ST[S, B] = flatMap(a => Pure(f(a)))

  def resume: Either[ST[S, A], A] = this match {
    case Pure(a) => Right(a)
    case More(k) => Left(k)
    case FlatMap(a, f) => a match {
      case Pure(v) => f(v).resume
      case More(k) => Left(FlatMap(k(), f))
      case FlatMap(b, g) => FlatMap(b, (x: Any) => FlatMap(g(x), f)).resume
    }
  }
  
  final def runT: A = resume match {
    case Right(a) => a
    case Left(k) => k.runT
  }
}

case class Pure[S, A](a: A) extends ST[S, A]
case class FlatMap[S, A, B](sub: ST[S, A], k: A => Trampoline[B]) extends ST[S, B]
case class More[S, A](thunk: ST[S, A]) extends ST[S, A]




object ST {
  def st[S, A](f: Tower[S] => Trampoline[(Tower[S], A)]): ST[S, A] = new ST[S, A] {
    def apply(s: Tower[S]) = f(s)
  }
  
}