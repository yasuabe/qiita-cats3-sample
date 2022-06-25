import cats.arrow.{ Category, Arrow }

//2. A Simple Arrow 
sealed case class SimpleFunc[A, B](runF: A => B)

given Arrow[SimpleFunc] with 
  def lift[A, B](f: A => B): SimpleFunc[A, B] = SimpleFunc(f)

  def first[A, B, C](fa: SimpleFunc[A, B]): SimpleFunc[(A, C), (B, C)] =
    SimpleFunc((a, c) => (fa runF a , c))

  def compose[A, B, C](g: SimpleFunc[B, C], f: SimpleFunc[A, B]): SimpleFunc[A, C] =
    SimpleFunc(g.runF compose f.runF)

// Category インスタンスは Arrow に含まれる

// 3. Some Arrow Operations
import cats.syntax.strong.*
import cats.syntax.arrow.*
import cats.syntax.compose.*

def arr[F[_, _]: Arrow, A, B](f: A => B): F[A, B] = Arrow[F] lift f

def split[F[_, _]: Arrow, A]: F[A, (A, A)] = arr(x => (x, x))

def unsplit[F[_, _]: Arrow, A, B, C](f: (A, B) => C): F[(A, B), C] =
  arr(f.tupled)

def liftA2_2[F[_, _]: Arrow, A, B, C, D](f: (B, C) => D, fb: F[A, B], fc: F[A, C])
  : F[A, D] = (fb &&& fc) >>> unsplit(f)

def liftA2[F[_, _]: Arrow, A, B, C, D](f: (B, C) => D, fb: F[A, B], fc: F[A, C])
  : F[A, D] = split[F, A] >>> fb.first >>> fc.second >>> unsplit(f)

// 4. An Example
val f: SimpleFunc[Int, Int] = arr(_ / 2)
val g: SimpleFunc[Int, Int] = arr(_ * 3 + 1)
val plus: (Int, Int) => Int = _ + _

val h = liftA2(plus, f, g)
h runF 8

liftA2_2(plus, f, g) runF 8
