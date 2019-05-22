// Andrzej Kolacz, 280009
// covariant immutable queues

import scala.annotation.tailrec


class MyQueue[+A] private (private[this] val front: List[A],
                           private[this] val rear:  List[A]){

  def enqueue[B >: A](y: B): MyQueue[B] =
    front match {
      case Nil => new MyQueue(y :: front, rear)
      case _   => new MyQueue(front, y :: rear)
    }

  def first: A =
    front match {
      case x :: _ => x
      case Nil    => throw new NoSuchElementException("first")
    }

  def firstOption: Option[A] =
    front match {
      case x :: _ => Some(x)
      case Nil    => None
    }

  def dequeue: MyQueue[A] =
    front match {
      case _ :: Nil => new MyQueue(rear.reverse, Nil)
      case _ :: xs  => new MyQueue(xs, rear)
      case Nil      => new MyQueue(Nil, Nil)
    }

  def isEmpty: Boolean = front == Nil
}


object MyQueue {
  def empty[A]         = new MyQueue[A](Nil, Nil)
  def apply[A](xs: A*) = new MyQueue[A](xs.toList, Nil)
}


sealed trait BT[+A]
case object Empty extends BT[Nothing]
case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]


object Lista3 extends App {
  def breadthBT[A](tree: BT[A]): List[A] = {

    @tailrec
    def bfs_aux(queue: MyQueue[BT[A]], acc: List[A]): List[A] = {
      if (!queue.isEmpty) {
        queue.first match {
          case Empty           => bfs_aux(queue.dequeue, acc)
          case Node(x, lt, rt) => bfs_aux(queue.dequeue.enqueue(lt).enqueue(rt), x :: acc)
        }
      } else acc.reverse
    }
    bfs_aux(MyQueue.apply(tree), Nil)
  }

  val t0 = Empty
  val t1 = Node(1, Node(2, Empty, Node(3, Empty, Empty)), Empty)
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

  println("BFS(t0): " + breadthBT[Int](t0).mkString(" "))
  println("BFS(t1): " + breadthBT[Int](t1).mkString(" "))
  println("BFS(t2): " + breadthBT[Int](t2).mkString(" "))
  println("BFS(t3): " + breadthBT[Int](t3).mkString(" "))

}
