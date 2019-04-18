// Andrzej Kołacz, 280009, lista 1
import scala.annotation.tailrec

// Zadanie 1

def reverse[A](xs: List[A]): List[A] = {
  @tailrec
  def reverse_aux(xs: List[A], acc: List[A]): List[A] = {
    xs match {
      case y :: ys => reverse_aux (ys, y :: acc)
      case Nil     => acc
    }
  }
  reverse_aux(xs, Nil)
}

reverse(List(1, 2, 3, 4))
reverse(Nil)
reverse(List(10, 9, 8, 7, 6, 5, 4, 3.1, 2, 1))

// Zadanie 2

def exists[A](xs: List[A])(p: A => Boolean): Boolean = {
  xs match {
    case y :: ys => p(y) || exists(ys)(p)
    case Nil     => false
  }
}

def div3(x: Int): Boolean = x % 3 == 0

exists(List(1, 2, 3, 4, 5))(_ == 2)
exists(List())(_ == 10)
exists(List(1, 2, 4, 8, 16, 32))(div3)
exists(List(1, 2, 4, 8, 16, 18, 32))(div3)

def existsL[A](xs: List[A])(p: A => Boolean): Boolean = {
  (xs foldLeft false)((acc, x) => acc || p(x))
}

existsL(List(1, 2, 3, 4, 5))(_ == 2)
existsL(List())(_ == 10)
existsL(List(1, 2, 4, 8, 16, 32))(div3)
existsL(List(1, 2, 4, 8, 16, 18, 32))(div3)

def existsR[A](xs: List[A])(p: A => Boolean): Boolean = {
  (xs foldRight false)((x, acc) => acc || p(x))
}

existsR(List(1, 2, 3, 4, 5))(_ == 2)
existsR(List())(_ == 10)
existsR(List(1, 2, 4, 8, 16, 32))(div3)
existsR(List(1, 2, 4, 8, 16, 18, 32))(div3)

// Zadanie 3

sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

val t = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)

def sumBT(t: BT[Int]): Int = {
  t match {
    case Node(x, lt, rt) => x + sumBT(lt) + sumBT(rt)
    case Empty           => 0
  }
}

sumBT(t)
sumBT(Empty)

// Zadanie 4

def foldBT[A, B](f: A => (B, B) => B)(acc: B)(bt: BT[A]): B = {
  bt match {
    case Node(x, lt, rt) => f (x) (foldBT(f)(acc)(lt), foldBT(f)(acc)(rt))
    case Empty           => acc
  }
}


// Zadanie 5

// a)

def sumBTfold(bt: BT[Int]): Int = {
  foldBT[Int, Int](x => (y,z) => x + y + z)(acc=0)(bt)
}


val t2 = Node(1, Empty, Empty)
val t3 = Node(10,
            Node(9,
              Node(8, Empty, Empty),
              Node(7, Empty, Empty)),
            Node(6,
              Node(5,
                Node(4, Empty, Empty),
                Node(3, Empty, Empty)),
              Node(2,
                Node(1, Empty, Empty),
                Node(0, Empty, Empty))))

sumBTfold(t)
sumBTfold(t2)
sumBTfold(t3)
sumBTfold(Empty)

// b)

def inorderBTfold[A](bt: BT[A]): List[A] = {
  foldBT[A, List[A]](x => (y, z) => x :: y ::: z)(acc=Nil)(bt)
}

inorderBTfold(t)
inorderBTfold(t2)
inorderBTfold(t3)
inorderBTfold(Empty)

// Zadanie 6

def mapBT[A, B](f: A => B)(tree: BT[A]): BT[B] = {
  foldBT[A, BT[B]](x => (y, z) => Node(f(x), y, z))(Empty)(tree)
}

mapBT((x: Int) => x * 2)(t)
mapBT((x: Int) => x * 2)(t2)
mapBT((x: Int) => x * 2)(t3)
mapBT((x: Int) => x * 2)(Empty)
