package net.akouryy

package hexd {

trait Arithmetic[A, B, C] {
  def +(a: A, b: B): C
  def -(a: A, b: B): C
  def *(a: A, b: B): C
  def /(a: A, b: B): C
}
object Arithmetic {
  implicit object IntPlusInt extends Arithmetic[Int, Int, Int] {
    def +(a: Int, b: Int) = a + b
    def -(a: Int, b: Int) = a - b
    def *(a: Int, b: Int) = a * b
    def /(a: Int, b: Int) = a / b
  }
  implicit object DoublePlusDouble extends Arithmetic[Double, Double, Double] {
    def +(a: Double, b: Double) = a + b
    def -(a: Double, b: Double) = a - b
    def *(a: Double, b: Double) = a * b
    def /(a: Double, b: Double) = a / b
  }
  implicit object IntPlusDouble extends Arithmetic[Int, Double, Double] {
    def +(a: Int, b: Double) = a + b
    def -(a: Int, b: Double) = a - b
    def *(a: Int, b: Double) = a * b
    def /(a: Int, b: Double) = a / b
  }
  implicit object DoublePlusInt extends Arithmetic[Double, Int, Double] {
    def +(a: Double, b: Int) = a + b
    def -(a: Double, b: Int) = a - b
    def *(a: Double, b: Int) = a * b
    def /(a: Double, b: Int) = a / b
  }
}

final case class V[Z, A](v: A) {
  def +[B,C](d: D[Z,B])(implicit e: Arithmetic[A,B,C]): V[Z,C] = V(e.+(v, d.d))
  def -[B,C](d: D[Z,B])(implicit e: Arithmetic[A,B,C]): V[Z,C] = V(e.-(v, d.d))
  def -[B,C](v2: V[Z,B])(implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e.-(v, v2.v))
  def axis[W] = new V[W, A](v)
  def to[B](implicit e: A => B) = V[Z, B](e(v))
}
case class D[Z,A](d: A) extends AnyVal {
  def +[B,C](v: V[Z,B]) (implicit e: Arithmetic[A,B,C]): V[Z,C] = V(e.+(d, v.v))
  def +[B,C](d2: D[Z,B])(implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e.+(d, d2.d))
  def -[B,C](d2: D[Z,B])(implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e.-(d, d2.d))
  def *[B,C](i: B)      (implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e.*(d, i))
  def *[B,C](r: R[Z,B]) (implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e.*(d, r.r))
  def /[B,C](i: B)      (implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e./(d, i))
  def axis[W] = new D[W, A](d)
  def to[B](implicit e: A => B) = D[Z, B](e(d))
}
final case class R[Z,A](r: A) {
  def *[B,C](d: D[Z,B])(implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e.*(r, d.d))
  def *[B,C](i: B)     (implicit e: Arithmetic[A,B,C]): R[Z,C] = R(e.*(r, i))
  def /[B,C](d: D[Z,B])(implicit e: Arithmetic[A,B,C]): D[Z,C] = D(e./(r, d.d))
  def /[B,C](i: B)     (implicit e: Arithmetic[A,B,C]): R[Z,C] = R(e./(r, i))
  def axis[W] = new R[W, A](r)
  def to[B](implicit e: A => B) = R[Z, B](e(r))
}

sealed trait Xs
sealed trait Ys
}

package object hexd {
  type VX[A] = V[Xs, A]
  type DX[A] = D[Xs, A]
  type RX[A] = R[Xs, A]
  type VY[A] = V[Ys, A]
  type DY[A] = D[Ys, A]
  type RY[A] = R[Ys, A]
  object VX {
    def apply[A](a: A) = V[Xs, A](a)
    def unapply[A](z: VX[A]) = V.unapply[Xs, A](z)
  }
  object DX {
    def apply[A](a: A) = D[Xs, A](a)
    def unapply[A](z: DX[A]) = D.unapply[Xs, A](z)
  }
  object RX {
    def apply[A](a: A) = R[Xs, A](a)
    def unapply[A](z: RX[A]) = R.unapply[Xs, A](z)
  }
  object VY {
    def apply[A](a: A) = V[Ys, A](a)
    def unapply[A](z: VY[A]) = V.unapply[Ys, A](z)
  }
  object DY {
    def apply[A](a: A) = D[Ys, A](a)
    def unapply[A](z: DY[A]) = D.unapply[Ys, A](z)
  }
  object RY {
    def apply[A](a: A) = R[Ys, A](a)
    def unapply[A](z: RY[A]) = R.unapply[Ys, A](z)
  }

  import language.implicitConversions

  implicit def to[Z, A, B](v: V[Z, A])(implicit e: A => B) = V[Z, B](e(v.v))
  implicit def to[Z, A, B](d: D[Z, A])(implicit e: A => B) = D[Z, B](e(d.d))
  implicit def to[Z, A, B](r: R[Z, A])(implicit e: A => B) = R[Z, B](e(r.r))
}
