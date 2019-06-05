// Andrzej Kolacz, 280009

// Zad. 1

class MyPair[A, B](var fst: A, var snd: B) {
  override def toString: String = "(" + fst + ", " + snd + ")"
}

// Zad. 2

class BankAccount(initialBalance: Double) {
  private[this] var balance: Double = initialBalance
  def checkBalance: Double = balance
  def  deposit(amount: Double): Double = { balance += amount; balance }
  def withdraw(amount: Double): Double = { balance -= amount; balance }
}

// a)
class CheckingAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  override def  deposit(amount: Double): Double = super.deposit(amount - 1)
  override def withdraw(amount: Double): Double = super.withdraw(amount + 1)
}

// b)
class SavingsAccount(initialBalance: Double) extends BankAccount(initialBalance) {
  private[this] var transactionsPerMonth: Int = 0
  def earnMonthlyInterest: Double = { super.deposit(checkBalance * 0.01); transactionsPerMonth = 0; checkBalance }
  override def deposit(amount: Double): Double = {
    transactionsPerMonth += 1
    super.deposit(if (transactionsPerMonth <= 3) amount else amount - 1)
  }
  override def withdraw(amount: Double): Double = {
    transactionsPerMonth += 1
    super.withdraw(if (transactionsPerMonth <= 3) amount else amount + 1)
  }
}

// Zad. 3 within the definition of Lista2.zad3 method

// Zad. 4
sealed trait lBT[+A]
case object LEmpty extends lBT[Nothing]
case class LNode[+A](elem: A, left: () => lBT[A], right: () => lBT[A]) extends lBT[A]


object Lista2 extends App {
  def zad1(): Unit = {
    println("Zad. 1: ")

    val a = new MyPair(fst = 1, snd = 2.0)
    println(a)
    a.fst_=(10)  // just checking if that works
    println("After the usage of a mutator: " + a)
    println()
  }

  def zad2(): Unit = {
    println("Zad. 2: ")

    println("a)")
    val checkingAccount = new CheckingAccount(100)
    println("Initial balance: " + checkingAccount.checkBalance)
    println("After withdraw(10): " + checkingAccount.withdraw(10))
    println("After deposit(200): " + checkingAccount.deposit(200))

    println("b)")
    val savingsAccount = new SavingsAccount(300)
    println("Initial balance: " + savingsAccount.checkBalance)
    for (k <- List(10, 20, 30, 40, 50)) {
      savingsAccount.deposit(k)
      println(s"After deposit($k): " + savingsAccount.checkBalance)
    }
    savingsAccount.earnMonthlyInterest
    println("After earnMonthlyInterest: " + savingsAccount.checkBalance)
    savingsAccount.deposit(10)
    println("After deposit(10): " + savingsAccount.checkBalance)
    println()
  }

  def zad3(): Unit = {
    def whileLoop (cond: => Boolean) (expr: => Unit): Unit = { // call by name; otherwise boolean value won't be updated
      if (cond) { expr; whileLoop (cond) (expr) }
    }

    println("Zad. 3")

    var a = 5
    whileLoop (a < 10) {
      println(a)
      a += 1
    }

    whileLoop (cond=false) {
      println(42)
    }

    println()
  }

  def zad4(): Unit = {
    // a)

    def lBreadth[A](ltree: lBT[A]): Stream[A] = {
      def lbfs_aux(queue: List[lBT[A]]): Stream[A] = {
        queue match {
          case Nil              => Stream.Empty
          case q_head :: q_tail =>
            q_head match {
              case LEmpty                   => lbfs_aux(q_tail)
              case LNode(elem, left, right) => elem #:: lbfs_aux(q_tail ::: (left() :: right() :: Nil))
          }
        }
      }
      lbfs_aux(ltree :: Nil)
    }

    // b)

    def lTree(n: Int): lBT[Int] = LNode(n, () => lTree(2 * n), () => lTree(2 * n + 1))

    println("A min-heap rooted in 2: " + lBreadth[Int](lTree(2)).take(15).toList)
    println("An empty tree: " + lBreadth[Int](LEmpty))
  }

  zad1()
  zad2()
  zad3()
  zad4()
}
