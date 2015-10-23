package com.squeryl.jdip.monads

import scala.annotation.tailrec

case class Tower[S]()



sealed trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}


/*sealed trait Free[S[_], A] {
  case class Done[S[_], A](a: A) extends Free[S, A]
  case class FlatMap[S[_], A, B](sub: Free[S, A], k: A => Free[S, B]) extends Free[S, B]
  case class More[S[_], A](thunk: S[Free[S, A]]) extends Free[S, A]
  
  def map[B](f: A => B): Free[S, B] = flatMap(a => Done(f(a)))
  def flatMap[B](f: A => Free[S, B]): Free[S, B] = FlatMap(this, f)
}*/

/*sealed trait Free[S[_], A] {  
  def map[B](f: A => B): Free[S, B] = flatMap(a => Done(f(a)))
  def flatMap[B](f: A => Free[S, B]): Free[S, B] = FlatMap(this, f)
}*/

object Free {
  sealed trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }
  
  case class Done[S[_], A](a: A) extends Free[S, A]
  case class FlatMap[S[_], A, B](sub: Free[S, A], k: A => Free[S, B]) extends Free[S, B]
  case class More[S[_], A](thunk: S[Free[S, A]]) extends Free[S, A]
  
  sealed trait Free[S[_], A] {  
    def map[B](f: A => B): Free[S, B] = flatMap(a => Done(f(a)))
    def flatMap[B](f: A => Free[S, B]): Free[S, B] = FlatMap(this, f)
    
    @tailrec final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] = 
    this match {
	    case Done(a) => Right(a )
	    case More(k) => Left(k)
	    case a FlatMap f => a match {
	      case Done(a) => f(a).resume
	      case More(k) => Left(S.map(k)( _ flatMap f ))
	      case b FlatMap g => b.flatMap (( x: Any ) =>
	      	g(x) flatMap f ). resume
	  	}
    }
  }
}

  /*def map[B](f: A => B) = flatMap(a => Done(f(a)))
  def flatMap[B](f: A => Free[S[_], B]) = FlatMap(this, f)
  
  @tailrec final def resume(implicit S: Functor[S]): Either[S[Free[S, A]], A] = 
    this match {
    case Done(a) => Right(a )
    case More(k) => Left(k)
    case a FlatMap f => a match {
      case Done(a) => f(a).resume
      case More(k) => Left(S.map(k)( _ flatMap f ))
      case b FlatMap g => b.flatMap (( x: Any ) =>
      	g(x) flatMap f ). resume
  	}
  }*/
//}





/*object Free {
	@tailrec final def resume[S[_], A](implicit S: Functor[S], free: Free[S, A]): Either[S[Free[S, A]], A] = 
    free match {
    case Done(a) => Right(a )
    case More(k) => Left(k)
    case a FlatMap f => a match {
      case Done(a) => f(a).resume
      case More(k) => Left(S.map(k)( _ flatMap f ))
      case b FlatMap g => b.flatMap (( x: Any ) =>
      	g(x) flatMap f ). resume
  	}
  }
}*/


/*sealed trait STRef[S, A] {
  protected var value: A
  
  def read: ST[S, A] = Pure[S, A](value)
  def mod(f: A => A): ST[S, STRef[S, A]] = (st: ST[S, A]) => {
    value = f(value)
    (s, this)
  }
  
  def write(a: A): ST[S, STRef[S, A]] = {
    value = f(value)
    (s, this)
  }
}*/



object ST {
  sealed trait ST[S, A] {
     def apply(s: S): (S, A)
     
     def map[B](g: A => B): ST[S, B] = 
       st(s => apply(s) match {
         case (ns, a) => (ns, g(a))
       })
  }
  
  implicit def applicativeST[S] = new Free.Functor[({type Q[A] = ST[S, A]})#Q] {
    def map[A, B](st: ST[S, A])(f: A => B): ST[S, B] = st.map(f)
  }
  
  type FreeST[S, A] = Free.Free[({type Q[A] =  ST[S, A]})#Q, A]
  def returnST[S, A](a: A): FreeST[S, A] = Free.Done[({type Q[A] = ST[S, A]})#Q, A](a)
  
  def st[S, A](f: S => (S, A)): ST[S, A] = new ST[S, A] {
    def apply(s: S) = f(s)
  }
  
  def evalS[S, A](s: S, t: Free.Free[({type Q[A] = ST[S, A]})#Q, A]): A = t.resume match {
    case Left(stm) => evalS(s, stm.asInstanceOf[ST[S, FreeST[S, A]]].apply(s)._2)
    case Right(a) => a.asInstanceOf[A]
  }
}

object Functions {
  def sequence[S, A](l: List[ST.FreeST[S, A]]) = l.foldLeft[ST.FreeST[S, List[A]]](ST.returnST(Nil))((u, v) =>
    (for (list <- u;
      elem <- v
    ) yield elem :: list).map(_.reverse)
  )
  
}