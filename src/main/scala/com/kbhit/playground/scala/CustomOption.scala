package com.kbhit.playground.scala

trait CustomOption[+A] {
  def map[B](f: A => B): CustomOption[B]

  def flatMap[B](f: A => CustomOption[B]): CustomOption[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => CustomOption[B]): CustomOption[B]

  def filter(f: A => Boolean): CustomOption[A]
}

case object CustomNone extends CustomOption[Nothing] {

  override def map[B](f: (Nothing) => B): CustomNone.type = CustomNone

  override def flatMap[B](f: (Nothing) => CustomOption[B]): CustomNone.type = CustomNone

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => CustomOption[B]): CustomOption[B] = ob

  override def filter(f: (Nothing) => Boolean): CustomNone.type = CustomNone

}

case class CustomSome[+A](get: A) extends CustomOption[A] {

  override def map[B](f: (A) => B) = CustomSome(f(get))

  override def flatMap[B](f: (A) => CustomOption[B]): CustomOption[B] =
    if (get != null) f(get) else CustomNone

  override def getOrElse[B >: A](default: => B): B =
    if (get != null) get else default

  override def orElse[B >: A](ob: => CustomOption[B]): CustomOption[B] =
    if (get != null) this else ob

  override def filter(f: (A) => Boolean): CustomOption[A] =
    if (f(get)) this else CustomNone

}

object CustomOption {

  def map2[A, B, C](a: CustomOption[A], b: CustomOption[B])(f: (A, B) => C): CustomOption[C] =
    for {
      x <- a
      y <- b
    } yield f(x, y)

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    Option(for {
      v <- a
      w <- v
    } yield w).filter(p => p.size == a.size)


}