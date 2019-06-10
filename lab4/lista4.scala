// Andrzej Kolacz, 280009
// concurrent programming basics

import java.util.concurrent.Semaphore


object Zad1a extends App {
  var counter = 0 // counter variable

  def readWriteCounter(): Unit = {
    // ********** CRITICAL SECTION *************
    // *****************************************
    val incrementedCounter = counter + 1 // reading counter
    counter = incrementedCounter // writing to counter
    // *****************************************
  }

  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())

  val startTime = System.nanoTime
  p.start(); q.start()
  p.join(); q.join()
  val estimatedTime = (System.nanoTime - startTime)/1000000

  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}


object Zad1b extends App {
  var counter = 0

  def readWriteCounter(): Unit = {
    this.synchronized {
      counter += 1
    }
  }

  val p = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for(_ <- 0 until 200000) readWriteCounter())

  val startTime = System.nanoTime
  p.start(); q.start()
  p.join(); q.join()

  val estimatedTime = (System.nanoTime - startTime)/1000000

  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}


object Zad1c extends App {
  var counter = 0
  val semaphore = new Semaphore(1)

  def readWriteCounter(): Unit = {
    semaphore.acquire()
    counter += 1
    semaphore.release()
  }

  val p = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())
  val q = new Thread(() => for (_ <- 0 until 200000) readWriteCounter())

  val startTime = System.nanoTime
  p.start(); q.start()
  p.join(); q.join()
  val estimatedTime = (System.nanoTime - startTime) / 1000000

  println(s"The value of counter = $counter")
  println(s"Estimated time = ${estimatedTime}ms, Available processors = ${Runtime.getRuntime.availableProcessors}")
}


object Zad2 extends App {
  def parallel[A, B](block1: => A, block2: => B): (A, B) = {
    var resultA: Option[A] = None
    var resultB: Option[B] = None

    def evalA(): Unit = { resultA = Some(block1) }
    def evalB(): Unit = { resultB = Some(block2) }

    val p = new Thread(() => evalA())
    val q = new Thread(() => evalB())

    p.start(); q.start(); p.join(); q.join()

    val (Some(v1), Some(v2)) = (resultA, resultB)
    (v1, v2)
  }

  println(parallel(
    { for (i <- 1 to 20) println(s"1: $i")
      "a" + 1 },
    { for (i <- 1 to 20) println(s"2: $i")
      "b" + 2 }))

  println(parallel(Thread.currentThread.getName, Thread.currentThread.getName))
}


object Zad3 extends App {
  def periodically(duration: Long, times: Int)(block: => Unit): Unit = {
    def perform(): Unit = {
      var t = times
      while (t > 0) { block; Thread.sleep(duration); t -= 1 }
    }

    val p = new Thread(() => perform())
    p.setDaemon(true)
    p.start()
  }

  periodically(1000, 5) { print("y ") }
  periodically(1000, 25) { print("x ") }
  Thread.sleep(10000)
  println("Done sleeping")

  /*
  "x" will not be displayed 25 times, because threads are running in the daemon mode,
  so process terminates as soon as it finishes the last instruction after ~10s.
   */
}